extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

macro_rules! err {
    ($meta: expr) => {
        syn::Error::new_spanned($meta, r#"expected `debug = "..."`"#).to_compile_error()
    };
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as syn::DeriveInput);
    let ident = &ast.ident;

    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
        ..
    }) = &ast.data
    {
        named
    } else {
        unimplemented!();
    };

    let generic_params = ast.generics.type_params();

    let generic_bounds = ast
        .generics
        .type_params()
        .map(|ty| quote! { #ty: std::fmt::Debug });

    let fields = fields.iter().map(|f| {
        let ident = &f.ident;
        for attr in &f.attrs {
            let nv = match attr.parse_meta() {
                Ok(syn::Meta::NameValue(nv)) => nv,
                Ok(meta) => return err!(meta),
                Err(e) => return e.to_compile_error(),
            };
            match &nv.lit {
                syn::Lit::Str(s) => {
                    let format = &s.value();
                    return quote! {
                        .field(stringify!(#ident), &format_args!(#format, &self.#ident))
                    };
                }
                _ => return err!(nv),
            }
        }
        quote! {
            .field(stringify!(#ident), &self.#ident)
        }
    });

    let expr = quote! {
        impl<#(#generic_bounds,)*> std::fmt::Debug for #ident<#(#generic_params,)*> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!(#ident))
                #(#fields)*
                .finish()
            }
        }
    };
    expr.into()
}
