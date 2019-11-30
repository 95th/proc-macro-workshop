extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Group, Ident, Literal, TokenStream as TokenStream2, TokenTree};
use std::iter::Peekable;
use syn::parse::{Parse, ParseStream, Result};
use syn::Token;

#[derive(Debug)]
struct Seq {
    ident: syn::Ident,
    start: isize,
    end: isize,
    body: TokenStream2,
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = input.parse()?;
        input.parse::<Token![in]>()?;
        let start = input.parse::<syn::LitInt>()?.base10_parse()?;
        input.parse::<Token![..]>()?;
        let end = input.parse::<syn::LitInt>()?.base10_parse()?;
        let body;
        syn::braced!(body in input);
        let body = body.parse()?;
        Ok(Self {
            ident,
            start,
            end,
            body,
        })
    }
}

impl Into<TokenStream> for Seq {
    fn into(self) -> TokenStream {
        let (inner, changed) = self.inner_pass();
        if changed {
            inner.into()
        } else {
            self.outer_pass(inner).into()
        }
    }
}

impl Seq {
    fn expand_group<I>(
        &self,
        tt: TokenTree,
        iter: &mut Peekable<I>,
        out: &mut Vec<TokenTree>,
    ) -> bool
    where
        I: Iterator<Item = TokenTree>,
    {
        match tt {
            TokenTree::Punct(c) if c.as_char() == '#' => match iter.peek() {
                Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Parenthesis => {
                    let tts = self.outer_pass(g.stream());
                    let new_g = Group::new(Delimiter::None, tts);
                    out.push(TokenTree::Group(new_g));
                    iter.next();
                    if let Some(TokenTree::Punct(c)) = iter.next() {
                        assert_eq!(c.as_char(), '*');
                    } else {
                        panic!("Expected repetition");
                    }
                    return true;
                }
                _ => out.extend(vec![TokenTree::Punct(c)]),
            },
            TokenTree::Group(g) => {
                let (tts, changed) = self.try_expand_inner(g.stream());
                let new_g = Group::new(g.delimiter(), tts);
                out.push(TokenTree::Group(new_g));
                return changed;
            }
            tt => out.push(tt),
        }
        false
    }

    fn inner_pass(&self) -> (TokenStream2, bool) {
        self.try_expand_inner(self.body.clone())
    }

    fn try_expand_inner(&self, tts: TokenStream2) -> (TokenStream2, bool) {
        let mut changed = false;
        let mut out = Vec::new();
        let mut iter = tts.into_iter().peekable();
        while let Some(tt) = iter.next() {
            if self.expand_group(tt, &mut iter, &mut out) {
                changed = true;
            }
        }
        (out.into_iter().collect(), changed)
    }

    fn outer_pass(&self, tts: TokenStream2) -> TokenStream2 {
        let mut out = TokenStream2::new();
        for n in self.start..self.end {
            out.extend(self.expand(tts.clone(), n));
        }
        out
    }

    fn expand(&self, tts: TokenStream2, n: isize) -> TokenStream2 {
        let mut out = Vec::new();
        let mut iter = tts.into_iter().peekable();
        while let Some(tt) = iter.next() {
            match tt {
                TokenTree::Group(g) => {
                    let stream = self.expand(g.stream(), n);
                    let mut group = Group::new(g.delimiter(), stream);
                    group.set_span(g.span());
                    out.push(TokenTree::Group(group));
                }
                TokenTree::Ident(ident) => {
                    if ident == self.ident {
                        let mut lit = Literal::isize_unsuffixed(n);
                        lit.set_span(ident.span());
                        out.push(TokenTree::Literal(lit));
                        continue;
                    }

                    if let Some(TokenTree::Punct(next)) = iter.peek() {
                        if next.as_char() == '#' {
                            iter.next();
                            if let Some(TokenTree::Ident(next)) = iter.peek() {
                                if next == &self.ident {
                                    iter.next();
                                    let mut new_name = format!("{}{}", ident, n);
                                    if let Some(TokenTree::Punct(next)) = iter.peek() {
                                        if next.as_char() == '#' {
                                            iter.next();
                                            if let Some(TokenTree::Ident(suffix)) = iter.next() {
                                                new_name = format!("{}{}{}", ident, n, suffix);
                                            }
                                        }
                                    }
                                    let new_ident = Ident::new(&new_name, ident.span());
                                    out.push(TokenTree::Ident(new_ident));
                                    continue;
                                }
                            }
                        }
                    }
                    out.push(TokenTree::Ident(ident));
                }
                tt => out.push(tt),
            }
        }
        out.into_iter().collect()
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let seq = syn::parse_macro_input!(input as Seq);
    seq.into()
}
