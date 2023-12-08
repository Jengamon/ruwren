use darling::{ast::NestedMeta, FromAttributes, FromDeriveInput, FromField, FromMeta};
use proc_macro2::Span;
use quote::{quote, quote_spanned};
use syn::{
    braced, parse::Parse, parse_macro_input, punctuated::Punctuated, spanned::Spanned, DeriveInput,
    ImplItem, ImplItemFn, Token, Type, Visibility,
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
    let wrapper_name = generate_wrapper_type_name(&wod.ident);
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
        quote! {
            #vis struct #wrapper_name<'a> {
                _marker: std::marker::PhantomData<&'a ()>,
                #(
                    #fvis #fnames: &'a mut #ftys
                ),*
            }
        }
    } else {
        quote! {
            #vis struct #wrapper_name<'a>(std::marker::PhantomData<&'a ()>, #(#fvis &'a mut #ftys),*);
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

    let expanded = quote! {
        #wrapper_type
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
