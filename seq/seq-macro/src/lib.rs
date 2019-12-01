extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Group, Ident, Literal, TokenStream as TokenStream2, TokenTree};
use proc_macro_hack::proc_macro_hack;
use std::iter::Peekable;
use syn::parse::{Parse, ParseStream, Result};
use syn::Token;

#[derive(Debug)]
struct Seq {
    ident: syn::Ident,
    start: isize,
    end: isize,
    inclusive: bool,
    body: TokenStream2,
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = input.parse()?;
        input.parse::<Token![in]>()?;
        let start = input.parse::<syn::LitInt>()?.base10_parse()?;
        let mut inclusive = true;
        if input.parse::<Token![..=]>().is_err() {
            input.parse::<Token![..]>()?;
            inclusive = false;
        }
        let end = input.parse::<syn::LitInt>()?.base10_parse()?;
        let body;
        syn::braced!(body in input);
        let body = body.parse()?;
        Ok(Self {
            ident,
            start,
            end,
            inclusive,
            body,
        })
    }
}

struct SeqIter {
    end: isize,
    curr: isize,
}

impl SeqIter {
    fn new(seq: &Seq) -> Self {
        let curr = seq.start;
        let end = if seq.inclusive { seq.end + 1 } else { seq.end };
        Self { end, curr }
    }
}

impl Iterator for SeqIter {
    type Item = isize;

    fn next(&mut self) -> Option<Self::Item> {
        if self.curr == self.end {
            None
        } else {
            let out = Some(self.curr);
            self.curr += 1;
            out
        }
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
    fn iter(&self) -> SeqIter {
        SeqIter::new(self)
    }

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
                    let (tts, changed) = self.try_expand_inner(g.stream());
                    let new_g = Group::new(Delimiter::None, tts);
                    out.push(TokenTree::Group(new_g));
                    iter.next();
                    if let Some(TokenTree::Punct(c)) = iter.next() {
                        assert_eq!(c.as_char(), '*');
                    } else {
                        panic!("Expected repetition");
                    }
                    return changed;
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
        for n in self.iter() {
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

#[proc_macro_hack]
pub fn eseq(input: TokenStream) -> TokenStream {
    seq(input)
}
