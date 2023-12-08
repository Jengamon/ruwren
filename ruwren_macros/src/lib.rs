use darling::{ast::NestedMeta, FromAttributes, FromDeriveInput, FromField, FromMeta};
use proc_macro2::Span;
use quote::{quote, quote_spanned};
use syn::{
    braced, parse::Parse, parse_macro_input, punctuated::Punctuated, spanned::Spanned, DeriveInput,
    ImplItem, ImplItemFn, Index, Token, Type, Visibility,
};

#[derive(FromField, Clone)]
#[darling(attributes(wren_field), forward_attrs(allow, doc, cfg))]
struct WrenObjectField {
    ident: Option<syn::Ident>,
    vis: Visibility,
    ty: Type,
    #[darling(default)]
    static_member: bool,
}

#[derive(FromDeriveInput)]
#[darling(
    supports(struct_any),
    attributes(wren_object),
    forward_attrs(allow, doc, cfg)
)]
struct WrenObjectDecl {
    ident: syn::Ident,
    vis: Visibility,
    data: darling::ast::Data<(), WrenObjectField>,
}

fn generate_wrapper_type_name(name: &syn::Ident) -> syn::Ident {
    syn::Ident::new(&format!("{name}Wrapper"), Span::call_site())
}

fn generate_wrapper_type(wod: &WrenObjectDecl) -> proc_macro2::TokenStream {
    let source = &wod.ident;
    let wrapper_name = generate_wrapper_type_name(&wod.ident);
    let class_name = generate_class_type_name(&wod.ident);
    let inst_name = generate_instance_type_name(&wod.ident);

    let vis = &wod.vis;
    let fields = wod
        .data
        .as_ref()
        .take_struct()
        .expect("only structs supported (for now)")
        .fields;

    let fnames: Option<Vec<_>> = fields.iter().map(|f| f.ident.clone()).collect();
    let ftys: Vec<_> = fields.iter().map(|f| f.ty.clone()).collect();
    let fvis: Vec<_> = fields.iter().map(|f| f.vis.clone()).collect();

    if let Some(fnames) = fnames {
        let fields: Vec<_> = fields
            .iter()
            .map(|f| {
                let name = &f.ident;
                let source = if f.static_member {
                    syn::Ident::new("class", Span::call_site())
                } else {
                    syn::Ident::new("instance", Span::call_site())
                };
                quote_spanned!(f.ident.span()=>
                    #name: &mut #source.#name
                )
            })
            .collect();

        quote! {
            #vis struct #wrapper_name<'a> {
                ___marker: std::marker::PhantomData<&'a ()>,
                #(
                    #fvis #fnames: &'a mut #ftys
                ),*
            }

            impl<'a> From<&'a mut #source> for #wrapper_name<'a> {
                fn from(src: &'a mut #source) -> Self {
                    Self {
                        ___marker: std::marker::PhantomData,
                        #(
                            #fnames: &mut src.#fnames
                        ),*
                    }
                }
            }

            impl<'a> From<(&'a mut #class_name, &'a mut #inst_name)> for #wrapper_name<'a> {
                fn from((class, instance): (&'a mut #class_name, &'a mut #inst_name)) -> Self {
                    Self {
                        ___marker: std::marker::PhantomData,
                        #(
                            #fields
                        ),*
                    }
                }
            }
        }
    } else {
        let indices: Vec<_> = (0..fields.len()).map(|i| Index::from(i)).collect();
        let fields: Vec<_> = fields
            .iter()
            .scan((0, 0), |(ref mut ci, ref mut ii), f| {
                let (index, source) = if f.static_member {
                    *ci += 1;
                    (
                        Index::from(*ci - 1),
                        syn::Ident::new("class", Span::call_site()),
                    )
                } else {
                    *ii += 1;
                    (
                        Index::from(*ii - 1),
                        syn::Ident::new("instance", Span::call_site()),
                    )
                };
                Some(quote_spanned!(f.ident.span()=>
                    &mut #source.#index
                ))
            })
            .collect();

        quote! {
            #vis struct #wrapper_name<'a>(std::marker::PhantomData<&'a ()>, #(#fvis &'a mut #ftys),*);

            impl<'a> From<&'a mut #source> for #wrapper_name<'a> {
                fn from(src: &'a mut #source) -> Self {
                    Self (
                        std::marker::PhantomData,
                        #(
                            &mut src.#indices
                        ),*
                    )
                }
            }

            impl<'a> From<(&'a mut #class_name, &'a mut #inst_name)> for #wrapper_name<'a> {
                fn from((class, instance): (&'a mut #class_name, &'a mut #inst_name)) -> Self {
                    Self (
                        std::marker::PhantomData,
                        #(
                            #fields
                        ),*
                    )
                }
            }
        }
    }
}

fn generate_class_type_name(name: &syn::Ident) -> syn::Ident {
    syn::Ident::new(&format!("{name}Class"), Span::call_site())
}

fn generate_class_type(wod: &WrenObjectDecl) -> proc_macro2::TokenStream {
    let source = &wod.ident;
    let class_name = generate_class_type_name(&wod.ident);
    let vis = &wod.vis;
    let fields = wod
        .data
        .as_ref()
        .take_struct()
        .expect("only structs supported (for now)")
        .fields;
    let all_fields: Vec<_> = fields
        .into_iter()
        .enumerate()
        .map(|(i, f)| (Index::from(i), f))
        .collect();
    let fields: Vec<_> = all_fields
        .iter()
        .filter_map(|(_, f)| if f.static_member { Some(f) } else { None })
        .collect();

    let fnames: Option<Vec<_>> = fields.iter().map(|f| f.ident.clone()).collect();
    let ftys: Vec<_> = fields.iter().map(|f| f.ty.clone()).collect();
    let fvis: Vec<_> = fields.iter().map(|f| f.vis.clone()).collect();

    if let Some(fnames) = fnames {
        quote! {
            #vis struct #class_name {
                #(
                    #fvis #fnames: #ftys
                ),*
            }

            impl From<#source> for #class_name {
                fn from(value: #source) -> Self {
                    Self {
                        #(
                            #fnames: value.#fnames
                        ),*
                    }
                }
            }

            impl ruwren::foreign_v2::V2Class for #class_name {
                fn name() -> &'static str {
                    stringify!(#source)
                }
            }
        }
    } else {
        let findices: Vec<_> = all_fields
            .iter()
            .filter_map(|(i, f)| if f.static_member { Some(i) } else { None })
            .collect();
        quote! {
            #vis struct #class_name(#(#fvis #ftys),*);

            impl From<#source> for #class_name {
                fn from(value: #source) -> Self {
                    Self (
                        #(
                            value.#findices
                        ),*
                    )
                }
            }

            impl ruwren::foreign_v2::V2Class for #class_name {
                fn name() -> &'static str {
                    stringify!(#source)
                }
            }
        }
    }
}

fn generate_instance_type_name(name: &syn::Ident) -> syn::Ident {
    syn::Ident::new(&format!("{name}Instance"), Span::call_site())
}

fn generate_instance_type(wod: &WrenObjectDecl) -> proc_macro2::TokenStream {
    let source = &wod.ident;
    let inst_name = generate_instance_type_name(&wod.ident);
    let vis = &wod.vis;
    let fields = wod
        .data
        .as_ref()
        .take_struct()
        .expect("only structs supported (for now)")
        .fields;
    let all_fields: Vec<_> = fields
        .into_iter()
        .enumerate()
        .map(|(i, f)| (Index::from(i), f))
        .collect();
    let fields: Vec<_> = all_fields
        .iter()
        .filter_map(|(_, f)| if !f.static_member { Some(f) } else { None })
        .collect();

    let fnames: Option<Vec<_>> = fields.iter().map(|f| f.ident.clone()).collect();
    let ftys: Vec<_> = fields.iter().map(|f| f.ty.clone()).collect();
    let fvis: Vec<_> = fields.iter().map(|f| f.vis.clone()).collect();

    if let Some(fnames) = fnames {
        quote! {
            #vis struct #inst_name {
                #(
                    #fvis #fnames: #ftys
                ),*
            }

            impl From<#source> for #inst_name {
                fn from(value: #source) -> Self {
                    Self {
                        #(
                            #fnames: value.#fnames
                        ),*
                    }
                }
            }
        }
    } else {
        let findices: Vec<_> = all_fields
            .iter()
            .filter_map(|(i, f)| if !f.static_member { Some(i) } else { None })
            .collect();
        quote! {
            #vis struct #inst_name(#(#fvis #ftys),*);

            impl From<#source> for #inst_name {
                fn from(value: #source) -> Self {
                    Self (
                        #(
                            value.#findices
                        ),*
                    )
                }
            }
        }
    }
}

#[proc_macro_derive(WrenObject, attributes(wren_object, wren_field))]
pub fn wren_object_derive(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(stream as DeriveInput);

    let wren_object_data = match WrenObjectDecl::from_derive_input(&input) {
        Ok(wod) => wod,
        Err(e) => {
            return proc_macro::TokenStream::from(e.write_errors());
        }
    };

    let wrapper_type = generate_wrapper_type(&wren_object_data);
    let class_type = generate_class_type(&wren_object_data);
    let instance_type = generate_instance_type(&wren_object_data);

    let expanded = quote! {
        #wrapper_type
        #class_type
        #instance_type
    };

    proc_macro::TokenStream::from(expanded)
}

#[derive(Default, FromAttributes)]
#[darling(default, attributes(wren_impl))]
struct WrenImplFnAttrs {
    // [0, 1] required (if 0, will attempt to use Default on Foo to generate FooClass)
    allocator: bool,
    // [0, 1] required (if 0, will attempt to use Default on Foo to generate FooInstance)
    constructor: bool,

    instance: bool,
    getter: bool,
    setter: bool,
}

struct WrenImplFn {
    func: ImplItemFn,
    attrs: WrenImplFnAttrs,
}

impl Parse for WrenImplFn {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let item: ImplItem = input.parse()?;
        match item {
            ImplItem::Fn(func) => {
                let attrs = WrenImplFnAttrs::from_attributes(&func.attrs)?;
                Ok(Self { func, attrs })
            }
            _ => unimplemented!(),
        }
    }
}

struct WrenObjectImpl {
    ty: syn::Type,
    items: Vec<WrenImplFn>,
}

impl Parse for WrenObjectImpl {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<Token![impl]>()?;
        let ty: syn::Type = input.parse()?;
        let content;
        braced!(content in input);
        let mut items = vec![];
        while !content.is_empty() {
            items.push(content.parse()?);
        }
        Ok(Self { ty, items })
    }
}

#[proc_macro_attribute]
pub fn wren_impl(
    _attr: proc_macro::TokenStream, item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let wren_object_impl = parse_macro_input!(item as WrenObjectImpl);

    let expanded = quote! {};

    proc_macro::TokenStream::from(expanded)
}

struct WrenModuleItem {
    ty: syn::Type,
}

impl Parse for WrenModuleItem {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<Token![pub]>()?;
        let ty: Type = input.parse()?;
        Ok(Self { ty })
    }
}

struct WrenModuleDecl {
    name: syn::Ident,
    items: Punctuated<WrenModuleItem, Token![;]>,
}

impl Parse for WrenModuleDecl {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<Token![mod]>()?;
        let name: syn::Ident = input.parse()?;
        let content;
        braced!(content in input);
        let items = content.parse_terminated(WrenModuleItem::parse, Token![;])?;
        Ok(Self { name, items })
    }
}

#[proc_macro]
pub fn wren_module(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let wren_module_decl = parse_macro_input!(stream as WrenModuleDecl);

    let expanded = quote! {};

    proc_macro::TokenStream::from(expanded)
}
