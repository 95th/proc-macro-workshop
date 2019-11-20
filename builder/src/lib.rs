extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as syn::DeriveInput);
    let ident = &ast.ident;

    let builder_name = format!("{}Builder", ident);
    let builder_ident = syn::Ident::new(&builder_name, ident.span());

    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
        ..
    }) = &ast.data
    {
        named
    } else {
        unimplemented!();
    };

    let is_option = |ty: &syn::Type| -> bool {
        if let syn::Type::Path(p) = ty {
            p.path.segments.len() == 1 && p.path.segments[0].ident == "Option"
        } else {
            false
        }
    };

    let opt_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if is_option(&ty) {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
    });

    let methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        quote! {
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });

    let builder_fields = fields.iter().map(|f| {
        let name = &f.ident;
        quote! { #name: None }
    });

    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;
        quote! {
            #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
        }
    });

    let expanded = quote! {
        pub struct #builder_ident {
            #(#opt_fields,)*
        }

        impl #ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#builder_fields,)*
                }
            }
        }

        impl #builder_ident {
            #(#methods)*

            pub fn build(&mut self) -> Result<#ident, Box<dyn std::error::Error>> {
                Ok(#ident {
                    #(#build_fields,)*
                })
            }
        }
    };

    expanded.into()
}
