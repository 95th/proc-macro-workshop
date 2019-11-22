extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

#[proc_macro_derive(Builder, attributes(builder))]
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

    let opt_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if get_inner_ty("Option", ty).is_some() {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
    });

    let methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if let Some(inner_ty) = get_inner_ty("Option", ty) {
            quote! {
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        } else {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        }
    });

    let each_methods = fields.iter().filter_map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        for attr in &f.attrs {
            if attr.path.segments.len() == 1 && attr.path.segments[0].ident == "builder" {
                if let Some(proc_macro2::TokenTree::Group(g)) = attr.tokens.clone().into_iter().next() {
                    let mut token = g.stream().into_iter();
                    match token.next().unwrap() {
                        proc_macro2::TokenTree::Ident(ident) => assert_eq!(ident, "each"),
                        tt => panic!("Expected 'each', found {}", tt),
                    }
                    match token.next().unwrap() {
                        proc_macro2::TokenTree::Punct(punct) => assert_eq!(punct.as_char(), '='),
                        tt => panic!("Expected '=', found {}", tt),
                    }
                    let arg = match token.next().unwrap() {
                        proc_macro2::TokenTree::Literal(lit) => lit,
                        tt => panic!("Expected literal, found {}", tt),
                    };
                    match syn::Lit::new(arg) {
                        syn::Lit::Str(s) => {
                            let ident = syn::Ident::new(&s.value(), s.span());
                            if let Some(inner_ty) = get_inner_ty("Vec", ty) {
                                return Some(quote! {
                                    pub fn #ident(&mut self, #ident: #inner_ty) -> &mut Self {
                                        if let Some(v) = self.#name {
                                            v.push(#ident);
                                        } else {
                                            self.#name = vec![#ident];
                                        }
                                        self
                                    }
                                });
                            }
                        },
                        tt => panic!("Expected string, found {:?}", tt)
                    }
                }
            }
        }
        None
    });

    let builder_fields = fields.iter().map(|f| {
        let name = &f.ident;
        quote! { #name: None }
    });

    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;
        if get_inner_ty("Option", &f.ty).is_some() {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
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

            #(#each_methods)*

            pub fn build(&mut self) -> Result<#ident, Box<dyn std::error::Error>> {
                Ok(#ident {
                    #(#build_fields,)*
                })
            }
        }
    };

    expanded.into()
}

fn get_inner_ty<'a>(outer: &str, ty: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(p) = ty {
        if p.path.segments.len() != 1 || p.path.segments[0].ident != outer {
            return None;
        }

        if let syn::PathArguments::AngleBracketed(inner_ty) = &p.path.segments[0].arguments {
            if inner_ty.args.len() != 1 {
                return None;
            }

            for arg in &inner_ty.args {
                return if let syn::GenericArgument::Type(t) = arg {
                    Some(t)
                } else {
                    None
                };
            }
        }
    }
    None
}
