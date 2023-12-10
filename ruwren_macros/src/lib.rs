use proc_macro2::Span;
use quote::{quote, quote_spanned, ToTokens};
use syn::{
    braced, parse::Parse, parse_macro_input, punctuated::Punctuated, spanned::Spanned, Data,
    DeriveInput, ImplItem, ImplItemFn, Index, Path, ReturnType, Token, Type, Visibility,
};

fn generate_wrapper_type_name(name: &syn::Ident) -> syn::Ident {
    syn::Ident::new(&format!("{name}Wrapper"), Span::call_site())
}

fn generate_class_type_name(name: &syn::Ident) -> syn::Ident {
    syn::Ident::new(&format!("{name}Class"), Span::call_site())
}

fn generate_instance_type_name(name: &syn::Ident) -> syn::Ident {
    syn::Ident::new(&format!("{name}Instance"), Span::call_site())
}

fn generate_class_type(tp: &syn::TypePath) -> syn::TypePath {
    let qself = tp.qself.clone();
    let mut path = tp.path.clone();
    let last_item = path
        .segments
        .last_mut()
        .expect(&format!("{:?} has no last component", tp));
    last_item.ident = generate_class_type_name(&last_item.ident);
    syn::TypePath { qself, path }
}

fn generate_instance_type(tp: &syn::TypePath) -> syn::TypePath {
    let qself = tp.qself.clone();
    let mut path = tp.path.clone();
    let last_item = path
        .segments
        .last_mut()
        .expect(&format!("{:?} has no last component", tp));
    last_item.ident = generate_instance_type_name(&last_item.ident);
    syn::TypePath { qself, path }
}

fn generate_class(
    name: &syn::Ident, fields: &syn::Fields, field_data: &[(&syn::Field, WrenObjectFieldDecl)],
) -> proc_macro2::TokenStream {
    let cname = generate_class_type_name(name);
    match fields {
        syn::Fields::Unit => {
            quote! {
                struct #cname;
            }
        }
        syn::Fields::Named(_) => {
            quote! {
                struct #cname;
            }
        }
        syn::Fields::Unnamed(_) => {
            quote! {
                struct #cname;
            }
        }
    }
}

fn generate_wrapper(
    name: &syn::Ident, fields: &syn::Fields, field_data: &[(&syn::Field, WrenObjectFieldDecl)],
) -> proc_macro2::TokenStream {
    let wname = generate_wrapper_type_name(name);
    quote! {}
}

/*
impl ForeignItem for FooInstance {
    type Class = FooClass;
    type Source = Foo;

    fn construct(class: &mut Self::Class, vm: &ruwren::VM) -> Self {
        let arg0_calc = InputSlot::new::<_, f64>(1, 1);
        // let arg1_calc = InputSlot::object_next(1, &arg0_calc);
        vm.ensure_slots(arg0_calc.scratch_end());
        let arg0 = get_slot_value(vm, &arg0_calc, 1);
        // let arg1 = get_slot_object::<Self>(vm, &arg1_calc, class);
        FooClass::construct(class, arg0)
    }
}
*/

fn generate_instance(
    name: &syn::Ident, fields: &syn::Fields, field_data: &[(&syn::Field, WrenObjectFieldDecl)],
) -> proc_macro2::TokenStream {
    let iname = generate_instance_type_name(name);
    match fields {
        syn::Fields::Unit => {
            quote! {
                struct #iname;

                impl From<#name> for #iname {
                    fn from(source: #name) -> Self {
                        Self
                    }
                }
            }
        }
        syn::Fields::Named(_) => {
            quote! {
                struct #iname;

                impl From<#name> for #iname {
                    fn from(source: #name) -> Self {
                        todo!("From impl for named-fields struct")
                    }
                }
            }
        }
        syn::Fields::Unnamed(_) => {
            quote! {
                struct #iname;

                impl From<#name> for #iname {
                    fn from(source: #name) -> Self {
                        todo!("From impl for unnamed-fields struct")
                    }
                }
            }
        }
    }
}

fn generate_enhancements(
    name: &syn::Ident, fields: &syn::Fields, field_data: &[(&syn::Field, WrenObjectFieldDecl)],
) -> proc_macro2::TokenStream {
    let class_name = generate_class_type_name(name);
    let instance_name = generate_instance_type_name(name);

    let from_impl = match fields {
        syn::Fields::Unit => {
            quote! {
                Self
            }
        }
        syn::Fields::Named(_) => {
            quote! {
                todo!("impl for structs with named fields")
            }
        }
        syn::Fields::Unnamed(_) => {
            quote! {
                todo!("impl for structs with unnnamed fields")
            }
        }
    };

    quote! {
        impl<'a> From<(&'a #class_name, &'a #instance_name)> for #name {
            fn from((class, instance): (&'a #class_name, &'a #instance_name)) -> Self {
                #from_impl
            }
        }

        impl TryFrom<Option<#name>> for #name {
            type Error = ();

            fn try_from(value: Option<#name>) -> Result<Self, Self::Error> {
                value.ok_or(())
            }
        }
    }
}

#[derive(deluxe::ExtractAttributes)]
#[deluxe(attributes(wren))]
struct WrenObjectFieldDecl {
    #[deluxe(default)]
    static_member: bool,
}

#[proc_macro_derive(WrenObject, attributes(wren))]
pub fn wren_object_derive(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(stream as DeriveInput);

    let mut struct_impl = match input.data {
        Data::Struct(s) => s,
        _ => {
            return quote! {
                compile_error!("only structs are supported")
            }
            .into()
        }
    };

    let errors = deluxe::Errors::new();

    let field_decls: Option<Vec<WrenObjectFieldDecl>> = struct_impl
        .fields
        .iter_mut()
        .map(|f| match deluxe::extract_attributes(f) {
            Ok(fd) => Some(fd),
            Err(e) => {
                errors.push_syn(e);
                None
            }
        })
        .collect();

    let field_decls = if let Some(field_decls) = field_decls {
        struct_impl.fields.iter().zip(field_decls).collect()
    } else {
        errors.push_call_site("A field decl extractor failed.");
        vec![]
    };

    let wrapper_type = generate_wrapper(&input.ident, &struct_impl.fields, &field_decls);
    let class_type = generate_class(&input.ident, &struct_impl.fields, &field_decls);
    let instance_type = generate_instance(&input.ident, &struct_impl.fields, &field_decls);
    let enhancements = generate_enhancements(&input.ident, &struct_impl.fields, &field_decls);

    let expanded = quote! {
        #errors
        #enhancements
        #wrapper_type
        #class_type
        #instance_type
    };

    proc_macro::TokenStream::from(expanded)
}

#[derive(Clone, Default, deluxe::ExtractAttributes)]
#[deluxe(default)]
struct WrenImplFnAttrs {
    // [0, 1] required (if 0, will attempt to use Default on Foo to generate FooClass)
    allocator: bool,
    // [0, 1] required (if 0, will attempt to use Default on Foo to generate FooInstance)
    constructor: bool,

    instance: bool,
    getter: bool,
    setter: bool,

    object: Vec<syn::Ident>,
}

#[derive(Clone)]
struct WrenImplFn {
    func: ImplItemFn,
    attrs: WrenImplFnAttrs,
}

impl WrenImplFn {
    fn validate_allocator(&mut self, ty: &syn::Ident) -> Result<(), Vec<String>> {
        let class_ty = generate_class_type_name(ty);

        let mut errors = vec![];

        if self.func.sig.inputs.len() > 0 {
            errors.push("allocators cannot take any parameters".to_string());
        }

        match self.func.sig.output {
            ReturnType::Default => {
                self.func.sig.output = ReturnType::Type(
                    Token![->](Span::call_site()),
                    Box::new(Type::Path(syn::TypePath {
                        qself: None,
                        path: syn::Path::from(class_ty.clone()),
                    })),
                )
            }
            ReturnType::Type(_, ref ty) => match ty.as_ref() {
                Type::Path(p) => {
                    let last = p.path.segments.last();
                    if last.is_none() || last.is_some_and(|name| name.ident != class_ty) {
                        errors.push(format!(
                            "allocators must return {}, but allocator returned {}",
                            class_ty.into_token_stream(),
                            p.into_token_stream()
                        ))
                    }
                }
                ty => errors.push(format!(
                    "allocators must return {}, but allocator returned {}",
                    class_ty.into_token_stream(),
                    ty.into_token_stream()
                )),
            },
        }

        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(())
        }
    }

    fn validate_constructor(&self, ty: &syn::Ident) -> Result<(), Vec<String>> {
        Ok(())
    }
}

impl Parse for WrenImplFn {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let item: ImplItem = input.parse()?;
        match item {
            ImplItem::Fn(mut func) => {
                let attrs = deluxe::extract_attributes(&mut func)?;
                Ok(Self { func, attrs })
            }
            _ => unimplemented!(),
        }
    }
}

struct WrenObjectImpl {
    ty: syn::Ident,
    items: Vec<WrenImplFn>,
}

struct WrenObjectValidImpl {
    ty: syn::Ident,
    allocator: Option<WrenImplFn>,
    constructor: Option<WrenImplFn>,
    items: Vec<WrenImplFn>,
}

impl WrenObjectImpl {
    fn validate(self) -> Result<WrenObjectValidImpl, Vec<String>> {
        let allocators: Vec<_> = self.items.iter().filter(|fi| fi.attrs.allocator).collect();
        let constructors: Vec<_> = self
            .items
            .iter()
            .filter(|fi| fi.attrs.constructor)
            .collect();
        let others: Vec<_> = self
            .items
            .iter()
            .filter(|fi| !fi.attrs.constructor && !fi.attrs.allocator)
            .cloned()
            .collect();

        let mut errors = vec![];

        let mut allocator = if allocators.len() <= 1 {
            allocators.first().cloned().cloned()
        } else {
            return Err(vec![format!(
                "Expected 0 or 1 allocators, found {}",
                allocators.len()
            )]);
        };

        if let Some(ref mut allocator) = allocator {
            match allocator.validate_allocator(&self.ty) {
                Err(errs) => errors.extend(errs.into_iter()),
                _ => {}
            }
        }

        let constructor = if constructors.len() <= 1 {
            constructors.first().cloned().cloned()
        } else {
            return Err(vec![format!(
                "Expected 0 or 1 constructors, found {}",
                constructors.len()
            )]);
        };

        if let Some(ref constructor) = constructor {
            match constructor.validate_constructor(&self.ty) {
                Err(errs) => errors.extend(errs.into_iter()),
                _ => {}
            }
        }

        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(WrenObjectValidImpl {
                ty: self.ty,
                allocator,
                constructor,
                items: others,
            })
        }
    }
}

impl Parse for WrenObjectImpl {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<Token![impl]>()?;
        let ty = input.parse()?;
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

    let errors = deluxe::Errors::new();

    let wren_object_impl = match wren_object_impl.validate() {
        Ok(valid) => valid,
        Err(errs) => {
            for err in errs {
                errors.push_call_site(err)
            }
            return quote! {
                #errors
            }
            .into();
        }
    };

    let source_ty = &wren_object_impl.ty;
    let instance_ty = generate_instance_type_name(source_ty);
    let class_ty = generate_class_type_name(source_ty);

    let allocator_fn = match &wren_object_impl.allocator {
        Some(alloc) => {
            let func = &alloc.func;
            quote! {
                #func
            }
        }
        None => quote! {
            fn ___default_alloc() -> #class_ty {
                Default::default()
            }
        },
    };

    let allocator_call = match &wren_object_impl.allocator {
        Some(alloc) => {
            let name = &alloc.func.sig.ident;
            quote! {
                #class_ty::#name()
            }
        }
        None => {
            quote! {
                #class_ty::___default_alloc()
            }
        }
    };

    let expanded = quote! {
        #errors
        impl #class_ty {
            #allocator_fn
        }

        impl ruwren::ClassObject for #instance_ty {
            fn initialize_pointer() -> extern "C" fn(*mut wren_sys::WrenVM)
            where
                Self: Sized,
            {
                todo!()
            }

            fn finalize_pointer() -> extern "C" fn(*mut std::ffi::c_void)
            where
                Self: Sized,
            {
                todo!()
            }

            fn generate_pointers() -> ruwren::ClassObjectPointers
            where
                Self: Sized,
            {
                todo!()
            }
        }

        impl ruwren::foreign_v2::V2Class for #class_ty {
            fn name() -> &'static str {
                stringify!(#source_ty)
            }
        }

        impl ruwren::foreign_v2::V2ClassAllocator for #class_ty {
            fn allocate() -> Self {
                #allocator_call
            }
        }

        impl ruwren::foreign_v2::ForeignItem for #instance_ty {
            type Class = #class_ty;
            type Source = #source_ty;

            fn construct(class: &mut Self::Class, vm: &ruwren::VM) -> Self {
                // let arg0_calc = InputSlot::new::<_, f64>(1, 1);
                // // let arg1_calc = InputSlot::object_next(1, &arg0_calc);
                // vm.ensure_slots(arg0_calc.scratch_end());
                // let arg0 = get_slot_value(vm, &arg0_calc, 1);
                // // let arg1 = get_slot_object::<Self>(vm, &arg1_calc, class);
                // FooClass::construct(class, arg0)
                todo!()
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}

struct WrenModuleItem {
    ty: syn::TypePath,
}

impl Parse for WrenModuleItem {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<Token![pub]>()?;
        let ty = input.parse()?;
        Ok(Self { ty })
    }
}

struct WrenModuleDecl {
    vis: syn::Visibility,
    name: syn::Ident,
    items: Punctuated<WrenModuleItem, Token![;]>,
}

impl Parse for WrenModuleDecl {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let vis = input.parse()?;
        input.parse::<Token![mod]>()?;
        let name: syn::Ident = input.parse()?;
        let content;
        braced!(content in input);
        let items = content.parse_terminated(WrenModuleItem::parse, Token![;])?;
        Ok(Self { vis, name, items })
    }
}

#[proc_macro]
pub fn wren_module(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let wren_module_decl = parse_macro_input!(stream as WrenModuleDecl);

    let vis = wren_module_decl.vis;
    let name = wren_module_decl.name;
    let (decls, to_impls): (Vec<_>, Vec<_>) = wren_module_decl
        .items
        .iter()
        .map(|mi| {
            let source_ty = &mi.ty;
            let class_ty = generate_class_type(source_ty);
            let instance_ty = generate_instance_type(source_ty);
            (
                quote_spanned! {mi.ty.span()=>
                    module.class::<#instance_ty, _>(#class_ty::name());
                },
                quote! {
                    impl WrenTo for #source_ty {
                        fn to_vm(self, vm: &ruwren::VM, slot: ruwren::SlotId, scratch_start: ruwren::SlotId) {
                            vm.set_slot_new_foreign_scratch::<_, _, #instance_ty>(
                                module_name(),
                                #class_ty::name(),
                                self.into(),
                                slot,
                                scratch_start,
                            )
                            .unwrap();
                        }
                    }
                },
            )
        })
        .unzip();

    let expanded = quote! {
        #vis mod #name {
            use ruwren::foreign_v2::{V2Class, WrenTo};

            fn module_name() -> String {
                stringify!(#name).replace("_", "/")
            }

            #(
                #to_impls
            )*

            pub fn publish_module(lib: &mut ruwren::ModuleLibrary) {
                let mut module = ruwren::Module::new();

                {
                    #(
                        #decls
                    )*
                }

                lib.module(module_name(), module);
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}
