use proc_macro2::Span;
use quote::{quote, quote_spanned, ToTokens};
use syn::{
    braced, parse::Parse, parse_macro_input, parse_quote, punctuated::Punctuated, spanned::Spanned,
    Data, DeriveInput, ImplItem, ImplItemFn, ReturnType, Token, Type,
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
            let valid: Vec<_> = field_data
                .iter()
                .filter_map(|(f, decl)| if decl.static_member { Some(*f) } else { None })
                .collect();
            let extract: Vec<_> = valid
                .iter()
                .map(|f| {
                    let name = f.ident.as_ref().unwrap();
                    quote_spanned! {f.span()=>
                        #name: source.#name
                    }
                })
                .collect();
            let decls: Vec<_> = valid
                .into_iter()
                .map(|f| {
                    // We can unwrap, because fields are definitely named
                    let name = f.ident.as_ref().unwrap();
                    let ty = &f.ty;
                    quote_spanned! {f.span()=>
                        #name: #ty
                    }
                })
                .collect();
            quote! {
                struct #cname {
                    #(
                        #decls
                    ),*
                }

                impl From<#name> for #cname {
                    fn from(source: #name) -> Self {
                        Self {
                            #(
                                #extract
                            ),*
                        }
                    }
                }
            }
        }
        syn::Fields::Unnamed(_) => {
            quote! {
                struct #cname;
            }
        }
    }
}

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
            let valid: Vec<_> = field_data
                .iter()
                .filter_map(|(f, decl)| if !decl.static_member { Some(*f) } else { None })
                .collect();
            let extract: Vec<_> = valid
                .iter()
                .map(|f| {
                    let name = f.ident.as_ref().unwrap();
                    quote_spanned! {f.span()=>
                        #name: source.#name
                    }
                })
                .collect();
            let decls: Vec<_> = valid
                .iter()
                .map(|f| {
                    // We can unwrap, because fields are definitely named
                    let name = f.ident.as_ref().unwrap();
                    let ty = &f.ty;
                    quote_spanned! {f.span()=>
                        #name: #ty
                    }
                })
                .collect();
            quote! {
                struct #iname {
                    #(
                        #decls
                    ),*
                }

                impl From<#name> for #iname {
                    fn from(source: #name) -> Self {
                        Self {
                            #(
                                #extract
                            ),*
                        }
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

fn generate_wrapper(name: &syn::Ident) -> proc_macro2::TokenStream {
    let wname = generate_wrapper_type_name(name);
    let iname = generate_instance_type_name(name);
    let cname = generate_class_type_name(name);

    quote! {
        struct #wname<'a> {
            class: &'a mut #cname,
            instance: &'a mut #iname,
        }

        impl<'a> From<(&'a mut #cname, &'a mut #iname)> for #wname<'a> {
            fn from((class, instance): (&'a mut #cname, &'a mut #iname)) -> Self {
                Self { class, instance }
            }
        }

        impl<'a> std::ops::Deref for #wname<'a> {
            type Target = #iname;
            fn deref(&self) -> &#iname {
                &self.instance
            }
        }

        impl<'a> std::ops::DerefMut for #wname<'a> {
            fn deref_mut(&mut self) -> &mut #iname {
                &mut self.instance
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
            let extract: Vec<_> = field_data
                .iter()
                .map(|(f, dat)| {
                    // We can unwrap, because fields are definitely named
                    let name = f.ident.as_ref().unwrap();
                    if dat.static_member {
                        quote! {
                            #name: class.#name.clone()
                        }
                    } else {
                        quote_spanned! {f.span()=>
                            #name: inst.#name.clone()
                        }
                    }
                })
                .collect();
            quote! {
                Self {
                    #(
                        #extract
                    ),*
                }
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
            fn from((class, inst): (&'a #class_name, &'a #instance_name)) -> Self {
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

    let class_type = generate_class(&input.ident, &struct_impl.fields, &field_decls);
    let instance_type = generate_instance(&input.ident, &struct_impl.fields, &field_decls);
    let enhancements = generate_enhancements(&input.ident, &struct_impl.fields, &field_decls);
    let wrapper_type = generate_wrapper(&input.ident);

    let expanded = quote! {
        #errors
        #enhancements
        #class_type
        #instance_type
        #wrapper_type
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

struct WrenImplValidFn {
    receiver_mut: bool,
    receiver_ty: syn::Type,
    is_static: bool,
    is_setter: bool,
    is_getter: bool,
    normal_params: Vec<(usize, syn::PatType)>,
    object_params: Vec<(usize, syn::PatType)>,
    func: ImplItemFn,
}

impl WrenImplValidFn {
    fn arity(&self) -> usize {
        self.normal_params.len() + self.object_params.len()
    }

    /// Generate the body for [`Self::gen_vm_fn()`] and [`Self::gen_vm_fn_constructor()`]
    fn gen_vm_fn_body(&self, source_name: &syn::Ident) -> proc_macro2::TokenStream {
        let (normal_extract, normal_arg): (Vec<_>, Vec<_>) = self
            .normal_params
            .iter()
            .map(|(idx, ty)| {
                let slot_idx = idx + 1;
                let arg_name = syn::Ident::new(&format!("arg{}", idx), Span::call_site());
                let arg_slot_name = syn::Ident::new(&format!("arg{}_calc", idx), Span::call_site());
                let ty = &*ty.ty;
                let arity = self.arity();
                let call = if *idx == 0 {
                    quote! {
                        new::<_, #ty>(#slot_idx, #arity)
                    }
                } else {
                    let prev_arg_slot_name =
                        syn::Ident::new(&format!("arg{}_calc", idx - 1), Span::call_site());

                    quote! {
                        next::<_, #ty>(#slot_idx, &#prev_arg_slot_name)
                    }
                };
                (
                    quote! {
                        let #arg_slot_name = ruwren::foreign_v2::InputSlot::#call
                    },
                    quote! {
                        let #arg_name: #ty = ruwren::foreign_v2::get_slot_value(vm, &#arg_slot_name, #arity)
                    },
                )
            })
            .unzip();
        let (object_extract, object_arg): (Vec<_>, Vec<_>) = self
        .object_params
        .iter()
        .map(|(idx, ty)| {
            let slot_idx = idx + 1;
            let arg_name = syn::Ident::new(&format!("arg{}", idx), Span::call_site());
            let arg_slot_name = syn::Ident::new(&format!("arg{}_calc", idx), Span::call_site());
            let ty = &*ty.ty;
            let source_type = match ty {
                syn::Type::Path(tp) => {
                    // let tp_original = tp.clone();
                    if let Some(last) = tp.path.segments.last() {
                        match &last.arguments {
                            syn::PathArguments::AngleBracketed(args) => {
                                if args.args.len() == 1 {
                                    match args.args.iter().find_map(|a| match a {
                                        syn::GenericArgument::Type(ty) => match ty {
                                            Type::Path(tp) => Some(generate_instance_type(tp)),
                                            _ => None,
                                        }
                                        _ => None,
                                    }) {
                                        Some(inst_ty) => {
                                            quote_spanned! {tp.span()=>
                                                #inst_ty
                                            }
                                        },
                                        None => quote! {
                                            compile_error!("invalid object type")
                                        }
                                    }                                  
                                } else {
                                    quote! {
                                        compile_error!("invalid object type")
                                    }
                                }
                            },
                            syn::PathArguments::None => {
                                let inst_ty = generate_instance_type(tp);
                                quote_spanned! {tp.span()=>
                                    #inst_ty
                                }
                            }
                            _ => quote! {
                                compile_error!("invalid object type")
                            }
                        }
                    } else {
                        quote! {
                            compile_error!("invalid object type")
                        }
                    }
                }
                _ => quote! {
                    compile_error!("invalid object type")
                }
            };
            let arity = self.arity();
            let call = if *idx == 0 {
                quote! {
                    object_new(#slot_idx, #arity)
                }
            } else {
                let prev_arg_slot_name =
                    syn::Ident::new(&format!("arg{}_calc", idx - 1), Span::call_site());

                quote! {
                    object_next(#slot_idx, &#prev_arg_slot_name)
                }
            };
            let receiver = if self.is_static {
                quote! {self}
            } else {
                quote! {self.class}
            };
            (
                quote! {
                    let #arg_slot_name = ruwren::foreign_v2::InputSlot::#call
                },
                quote! {
                    let #arg_name = ruwren::foreign_v2::get_slot_object::<#source_type>(vm, &#arg_slot_name, #arity, #receiver)
                },
            )
        })
        .unzip();

        let last_arg_check = if self.arity() > 0 {
            let last_arg = syn::Ident::new(&format!("arg{}_calc", self.arity() - 1), Span::call_site());
            quote! {
                vm.ensure_slots(#last_arg.scratch_end())
            }
        } else {
            quote!{}
        };

        let call = {
            let mut call_args: Vec<_> = self.object_params.iter().map(|(i, d)| (i, d, true)).chain(self.normal_params.iter().map(|(i, d)| (i, d, false))).collect();
            call_args.sort_by(|(a, _, _), (b, _, _)| a.cmp(b));
            let input_args = call_args.into_iter().map(|(idx, dat, is_obj)| {
                let arg_name = syn::Ident::new(&format!("arg{}", idx), Span::call_site());
                let ty = &dat.ty;
                if is_obj {
                quote! {
                    match #arg_name.try_into() {
                        Ok(v) => v, 
                        Err(_) => panic!(
                            "slot {} cannot be type {}",
                            2,
                            std::any::type_name::<#ty>()
                        ),
                    }
                }
            } else {
                quote! {
                    #arg_name
                }
            }});

            let class_name = generate_class_type_name(source_name);
            let wrapper_name = generate_wrapper_type_name(source_name);
            let name = &self.func.sig.ident;

            if self.is_static {
                quote! {
                    #class_name::#name(self, #(#input_args),*)
                }
            } else {
                quote! {
                    #wrapper_name::#name(self, #(#input_args),*)
                }
            }
        };

        quote! {
            #(
                #normal_extract
            );*;
            #(
                #object_extract
            );*;
            #last_arg_check;

            #(
                #normal_arg
            );*;
            #(
                #object_arg
            );*;
            let ret = #call;
        }
    }

    /// Generate a wrapper around this function that takes a receiver
    /// and the vm as arguments and returns an instance
    fn gen_vm_fn_constructor(&self, source_name: &syn::Ident) -> proc_macro2::TokenStream {
        let wrapper_fn_name =
            syn::Ident::new(&format!("vm_{}", self.func.sig.ident), Span::call_site());
        let instance_name = generate_instance_type_name(source_name);
        let vis = &self.func.vis;
        let body = self.gen_vm_fn_body(source_name);
        if self.receiver_mut {
            quote! {
                #vis fn #wrapper_fn_name(&mut self, vm: &ruwren::VM) -> #instance_name {
                    #body
                    ret
                }
            }
        } else {
            quote! {
                #vis fn #wrapper_fn_name(&self, vm: &ruwren::VM) -> #instance_name {
                    #body
                    ret
                }
            }
        }
    }

    /// Generate a wrapper around this function that takes a receiver
    /// and the vm as arguments
    fn gen_vm_fn(&self, source_name: &syn::Ident) -> proc_macro2::TokenStream {
        let wrapper_fn_name =
            syn::Ident::new(&format!("vm_{}", self.func.sig.ident), Span::call_site());
        let vis = &self.func.vis;
        let body = self.gen_vm_fn_body(source_name);
        if self.receiver_mut {
            quote! {
                #vis fn #wrapper_fn_name(&mut self, vm: &ruwren::VM) {
                    #body
                    ruwren::foreign_v2::WrenTo::to_vm(ret, vm, 0, 1)
                }
            }
        } else {
            quote! {
                #vis fn #wrapper_fn_name(&self, vm: &ruwren::VM) {
                    #body
                    ruwren::foreign_v2::WrenTo::to_vm(ret, vm, 0, 1)
                }
            }
        }
    }

    /// Generate a wrapper around this function that takes a
    /// *mut wren_sys::WrenVM as an argument.
    ///
    /// Calls [`Self::gen_vm_fn()`] internally to generate the function that
    /// this wrapper calls.
    ///
    /// This wrapper function is FFI-safe. (or at least, should be)
    fn gen_native_vm_fn(&self, source_name: &syn::Ident) -> proc_macro2::TokenStream {
        let wrapper_fn = self.gen_vm_fn(source_name);
        let wrapper_fn_name =
            syn::Ident::new(&format!("vm_{}", self.func.sig.ident), Span::call_site());
        let native_name = syn::Ident::new(
            &format!("native_vm_{}", self.func.sig.ident),
            Span::call_site(),
        );
        let instance_name = generate_instance_type_name(source_name);
        let class_name = generate_class_type_name(source_name);
        let wrapper_name = generate_wrapper_type_name(source_name);
        let vis = &self.func.vis;
        let native_wrapper = if self.is_static {
            quote! {
                #vis unsafe extern "C" fn #native_name(vm: *mut ruwren::wren_sys::WrenVM) {
                    use std::panic::{catch_unwind, set_hook, take_hook, AssertUnwindSafe};

                    let conf = std::ptr::read_unaligned(
                        ruwren::wren_sys::wrenGetUserData(vm) as *mut ruwren::UserData
                    );
                    let ovm = vm;
                    let vm = std::rc::Weak::upgrade(&conf.vm)
                        .expect(&format!("Failed to access VM at {:p}", &conf.vm));
                    set_hook(Box::new(|_| {}));
                    let vm_borrow = AssertUnwindSafe(vm.borrow());
                    match catch_unwind(|| {
                        use ruwren::foreign_v2::V2Class;
                        vm_borrow.use_class::<#instance_name, _, _>(|vm, cls| {
                            let class =
                                cls.expect(&format!("Failed to resolve class for {}", #class_name::name()));
                            #class_name::#wrapper_fn_name(class, vm)
                        })
                    }) {
                        Ok(_) => (),
                        Err(err) => {
                            let err_string = if let Some(strg) = err.downcast_ref::<String>() {
                                strg.clone()
                            } else if let Some(strg) = err.downcast_ref::<&str>() {
                                strg.to_string()
                            } else {
                                "Non-string panic message".into()
                            };

                            vm_borrow.set_slot_string(0, err_string);
                            vm_borrow.abort_fiber(0);
                        }
                    };
                    drop(take_hook());
                    std::ptr::write_unaligned(
                        ruwren::wren_sys::wrenGetUserData(ovm) as *mut ruwren::UserData,
                        conf,
                    );
                }
            }
        } else {
            let access = if self.receiver_mut {
                quote! {
                    let class =
                        cls.expect(&format!("Failed to resolve class for {}", #class_name::name()));
                    let mut wrapper: #wrapper_name = (class, inst).into();
                    wrapper.#wrapper_fn_name(vm)
                }
            } else {
                quote! {
                    let class =
                        cls.expect(&format!("Failed to resolve class for {}", #class_name::name()));
                    let wrapper: #wrapper_name = (class, inst).into();
                    wrapper.#wrapper_fn_name(vm)
                }
            };
            quote! {
                #vis unsafe extern "C" fn #native_name(vm: *mut wren_sys::WrenVM) {
                    use std::panic::{catch_unwind, set_hook, take_hook, AssertUnwindSafe};

                    let conf = std::ptr::read_unaligned(
                        ruwren::wren_sys::wrenGetUserData(vm) as *mut ruwren::UserData
                    );
                    let ovm = vm;
                    let vm = std::rc::Weak::upgrade(&conf.vm)
                        .expect(&format!("Failed to access VM at {:p}", &conf.vm));
                    set_hook(Box::new(|_pi| {}));
                    let vm_borrow = AssertUnwindSafe(vm.borrow());
                    match catch_unwind(|| {
                        use ruwren::foreign_v2::V2Class;
                        vm_borrow.ensure_slots(1);
                        let inst = vm_borrow
                            .get_slot_foreign_mut::<#instance_name>(0)
                            .expect(&format!(
                                "Tried to call {0} of {1} on non-{1} type",
                                stringify!($inf),
                                std::any::type_name::<#instance_name>()
                            ));
                        vm_borrow.use_class::<#instance_name, _, _>(|vm, cls| {
                            #access
                        })
                    }) {
                        Ok(_) => (),
                        Err(err) => {
                            let err_string = if let Some(strg) = err.downcast_ref::<String>() {
                                strg.clone()
                            } else if let Some(strg) = err.downcast_ref::<&str>() {
                                strg.to_string()
                            } else {
                                "Non-string panic message".into()
                            };

                            vm_borrow.set_slot_string(0, err_string);
                            vm_borrow.abort_fiber(0);
                        }
                    };
                    drop(take_hook());
                    std::ptr::write_unaligned(
                        ruwren::wren_sys::wrenGetUserData(ovm) as *mut ruwren::UserData,
                        conf,
                    );
                }
            }
        };

        quote! {
            #wrapper_fn
            #native_wrapper
        }
    }
}

#[derive(Clone)]
struct WrenImplFn {
    func: ImplItemFn,
    attrs: WrenImplFnAttrs,
}

impl TryFrom<(&syn::Ident, WrenImplFn)> for WrenImplValidFn {
    type Error = Vec<String>;

    fn try_from((src, value): (&syn::Ident, WrenImplFn)) -> Result<Self, Self::Error> {
        let (receiver_mut, receiver_ty, args): (_, syn::Type, _) =
            if let Some(rec) = value.func.sig.receiver() {
                let class_type = generate_class_type_name(src);
                let wrapper_type = generate_wrapper_type_name(src);
                (
                    rec.mutability.is_some(),
                    if value.attrs.instance {
                        parse_quote!( #wrapper_type<'a> )
                    } else {
                        parse_quote!( #class_type )
                    },
                    value.func.sig.inputs.clone(),
                )
            } else {
                let Some(arg) = value
                    .func
                    .sig
                    .inputs
                    .iter()
                    .nth(0)
                    .and_then(|fna| match fna {
                        syn::FnArg::Typed(aty) => Some(aty),
                        _ => None,
                    })
                else {
                    return Err(vec![format!(
                        "method {} must have a receiver",
                        value.func.sig.ident
                    )]);
                };
                let inputs = value.func.sig.inputs.clone().into_iter().skip(1).collect();

                let is_mut = match &*arg.ty {
                    syn::Type::Reference(r) => r.mutability.is_some(),
                    _ => {
                        return Err(vec![format!(
                            "method {} receiver must be a reference",
                            value.func.sig.ident
                        )])
                    }
                };

                (is_mut, (*arg.ty).clone(), inputs)
            };

        let object_param_pairs: Vec<_> = value
            .attrs
            .object
            .iter()
            .map(|name| {
                (
                    name,
                    args.iter().find(|i| match i {
                        syn::FnArg::Receiver(_) => false,
                        syn::FnArg::Typed(ty) => match &*ty.pat {
                            syn::Pat::Ident(i) => i.ident == *name,
                            _ => false,
                        },
                    }),
                )
            })
            .collect();

        let (object_params, normal_params): (Vec<_>, Vec<_>) = args
            .iter()
            .filter_map(|fna| match fna {
                syn::FnArg::Receiver(_) => None,
                syn::FnArg::Typed(ty) => Some(ty),
            })
            .cloned()
            .enumerate()
            .partition(|(_, arg)| match &*arg.pat {
                syn::Pat::Ident(i) => value.attrs.object.contains(&i.ident),
                _ => false,
            });

        let mut errors: Vec<_> = object_param_pairs
            .into_iter()
            .filter_map(|(name, arg)| {
                if arg.is_none() {
                    Some(format!("Could not find top-level object argument {}", name))
                } else {
                    None
                }
            })
            .collect();

        let is_setter = if value.attrs.setter {
            let output = &value.func.sig.output;
            // args.len() *counts* the receiver, so it is limit + 1
            if args.len() == 2
                && (*output == syn::ReturnType::Default || *output == parse_quote! { -> ()})
            {
                true
            } else {
                errors.push(format!(
                    "setter {} must take 1 non-receiver argument (takes {}), and return () (returns {})",
                    value.func.sig.ident,
                    args.len(),
                    match output {
                        syn::ReturnType::Default => parse_quote!{()},
                        syn::ReturnType::Type(_, ty) => ty.into_token_stream(),
                    }
                ));
                false
            }
        } else {
            false
        };

        let is_getter = if value.attrs.getter {
            let output = &value.func.sig.output;
            // args.len() *counts* the receiver, so it is limit + 1
            if args.len() == 1
                && *output != syn::ReturnType::Default
                && *output != parse_quote! { -> () }
            {
                true
            } else {
                errors.push(format!(
                    "getter {} must take no non-receiver arguments (takes {}), and return something (returns {})",
                    value.func.sig.ident,
                    args.len(),
                    match output {
                        syn::ReturnType::Default => parse_quote!{()},
                        syn::ReturnType::Type(_, ty) => ty.into_token_stream(),
                    }
                ));
                false
            }
        } else {
            false
        };

        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(Self {
                receiver_mut,
                receiver_ty,
                is_getter,
                is_setter,
                is_static: !value.attrs.instance,
                func: value.func,
                normal_params,
                object_params,
            })
        }
    }
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
                self.func.sig.output = parse_quote! { -> #class_ty };
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
    constructor: Option<WrenImplValidFn>,
    others: Vec<WrenImplValidFn>,
}

impl WrenObjectImpl {
    fn validate(self) -> Result<WrenObjectValidImpl, Vec<String>> {
        let allocators: Vec<_> = self.items.iter().filter(|fi| fi.attrs.allocator).collect();
        let constructors: Vec<_> = self
            .items
            .iter()
            .filter(|fi| fi.attrs.constructor)
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

        let constructor = if let Some(constructor) = constructor {
            let instance_name = generate_instance_type_name(&self.ty);
            let class_name = generate_class_type_name(&self.ty);
            match TryInto::<WrenImplValidFn>::try_into((&self.ty, constructor)) {
                Ok(constructor) => {
                    if constructor.func.sig.output == parse_quote! {-> #instance_name} {
                        if constructor.receiver_ty == parse_quote! { #class_name } {
                            Some(constructor)
                        } else {
                            errors.push(format!(
                                "A constructor must receive &mut {0} (or &{0}), but it receives {1}",
                                class_name.into_token_stream(),
                                constructor.receiver_ty.into_token_stream(),
                            ));
                            None
                        }
                    } else {
                        errors.push(format!(
                            "A constructor must return {}, but it returns {}",
                            instance_name.into_token_stream(),
                            constructor.func.sig.output.into_token_stream(),
                        ));
                        None
                    }
                }
                Err(errs) => {
                    errors.extend(errs.into_iter());
                    None
                }
            }
        } else {
            None
        };

        let others: Vec<_> = self
            .items
            .iter()
            .filter(|fi| !fi.attrs.constructor && !fi.attrs.allocator)
            .cloned()
            .filter_map(|func| -> Option<WrenImplValidFn> {
                match (&self.ty, func).try_into() {
                    Ok(vfunc) => Some(vfunc),
                    Err(errs) => {
                        errors.extend(errs.into_iter());
                        None
                    }
                }
            })
            .collect();

        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(WrenObjectValidImpl {
                ty: self.ty,
                allocator,
                constructor,
                others,
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
    let wrapper_ty = generate_wrapper_type_name(source_ty);

    let allocator_fn = match &wren_object_impl.allocator {
        Some(alloc) => {
            let func = &alloc.func;
            quote! {
                #func
            }
        }
        None => quote! {
            fn ___default_alloc() -> #class_ty {
                use std::default::Default;
                #source_ty::default().into()
            }
        },
    };

    let constructor_fn = match &wren_object_impl.constructor {
        Some(constructor) => {
            let func = &constructor.func;
            let wrapper_func = constructor.gen_vm_fn_constructor(source_ty);
            quote! {
                #wrapper_func
                #func
            }
        }
        None => quote! {
            fn ___default_constructor(&self) -> #instance_ty {
                use std::default::Default;
                #source_ty::default().into()
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

    let constructor_call = match &wren_object_impl.constructor {
        Some(constructor) => {
            let name = &constructor.func.sig.ident;
            let wrapper_name = syn::Ident::new(&format!("vm_{}", name), Span::call_site());
            quote! {
                #class_ty::#wrapper_name(class, vm)
            }
        }
        None => {
            quote! {
                #class_ty::___default_constructor(class)
            }
        }
    };

    let function_decls = wren_object_impl.others.iter().map(|func| {
        let name = &func.func.sig.ident;
        let wrapper_name = syn::Ident::new(&format!("native_vm_{}", name), Span::call_site());
        let is_static = func.is_static;
        let arity = func.arity();
        let receiver_ty = if func.is_static {
            &class_ty
        } else {
            &wrapper_ty
        };
        let sig = if func.is_getter {
            quote! { ruwren::FunctionSignature::new_getter(stringify!(#name)) }
        } else if func.is_setter {
            quote! { ruwren::FunctionSignature::new_setter(stringify!(#name)) }
        } else {
            quote! { ruwren::FunctionSignature::new_function(stringify!(#name), #arity) }
        };
        quote! {
            ruwren::MethodPointer {
                is_static: #is_static,
                signature: #sig,
                pointer: #receiver_ty::#wrapper_name,
            }
        }
    });

    let static_fns = wren_object_impl
        .others
        .iter()
        .filter(|of| of.is_static)
        .map(|func| {
            let wrapper_func = func.gen_native_vm_fn(source_ty);
            let func = &func.func;
            quote_spanned! {func.span()=>
                #wrapper_func
                #func
            }
        });

    let instance_fns = wren_object_impl
        .others
        .iter()
        .filter(|of| !of.is_static)
        .map(|func| {
            let wrapper_func = func.gen_native_vm_fn(source_ty);
            let func = &func.func;
            quote_spanned! {func.span()=>
                #wrapper_func
                #func
            }
        });

    let expanded = quote! {
        #errors
        impl #class_ty {
            #allocator_fn
            #constructor_fn
            #(
                #static_fns
            )*
        }

        impl<'a> #wrapper_ty<'a> {
            #(
                #instance_fns
            )*
        }

        impl ruwren::foreign_v2::Slottable<#source_ty> for #instance_ty {
            type Context = #class_ty;
            fn scratch_size() -> usize
            where
                Self: Sized,
            {
                0
            }
        
            fn get(
                ctx: &mut Self::Context, vm: &ruwren::VM, slot: ruwren::SlotId,
                _scratch_start: ruwren::SlotId,
            ) -> Option<#source_ty> {
                let inst = vm.get_slot_foreign::<Self>(slot)?;
                Some((&*ctx, inst).into())
            }
        }

        impl ruwren::ClassObject for #instance_ty {
            fn initialize_pointer() -> extern "C" fn(*mut wren_sys::WrenVM)
            where
                Self: Sized,
            {
                extern "C" fn _constructor(vm: *mut wren_sys::WrenVM) {
                    use ruwren::Class;
                    use std::panic::{catch_unwind, set_hook, take_hook, AssertUnwindSafe};
                    unsafe {
                        let conf = std::ptr::read_unaligned(
                            ruwren::wren_sys::wrenGetUserData(vm) as *mut ruwren::UserData
                        );
                        let ovm = vm;
                        let vm = std::rc::Weak::upgrade(&conf.vm)
                            .expect(&format!("Failed to access VM at {:p}", &conf.vm));
                        let wptr = ruwren::wren_sys::wrenSetSlotNewForeign(
                            vm.borrow().vm,
                            0,
                            0,
                            std::mem::size_of::<ruwren::ForeignObject<#instance_ty>>()
                                as ruwren::wren_sys::size_t,
                        );
                        // Allocate a new object, and move it onto the heap
                        set_hook(Box::new(|_pi| {}));
                        let vm_borrow = AssertUnwindSafe(vm.borrow());
                        let object = match catch_unwind(|| <#instance_ty as Class>::initialize(&*vm_borrow))
                        {
                            Ok(obj) => Some(obj),
                            Err(err) => {
                                let err_string = if let Some(strg) = err.downcast_ref::<String>() {
                                    strg.clone()
                                } else if let Some(strg) = err.downcast_ref::<&str>() {
                                    strg.to_string()
                                } else {
                                    "Non-string panic message".into()
                                };

                                vm_borrow.set_slot_string(0, err_string);
                                vm_borrow.abort_fiber(0);
                                None
                            }
                        };
                        drop(take_hook());
                        // Copy the object pointer if we were successful
                        if let Some(object) = object {
                            std::ptr::write(
                                wptr as *mut _,
                                ruwren::ForeignObject {
                                    object: Box::into_raw(Box::new(object)),
                                    type_id: std::any::TypeId::of::<#instance_ty>(),
                                },
                            );
                        }
                        std::ptr::write_unaligned(
                            ruwren::wren_sys::wrenGetUserData(ovm) as *mut ruwren::UserData,
                            conf,
                        );
                    }
                }
                _constructor
            }

            fn finalize_pointer() -> extern "C" fn(*mut std::ffi::c_void)
            where
                Self: Sized,
            {
                extern "C" fn _destructor(data: *mut std::ffi::c_void) {
                    unsafe {
                        let mut fo: ruwren::ForeignObject<#instance_ty> =
                            std::ptr::read_unaligned(data as *mut _);
                        if !fo.object.is_null() {
                            _ = Box::from_raw(fo.object);
                        }
                        fo.object = std::ptr::null_mut();
                        std::ptr::write_unaligned(data as *mut _, fo);
                    }
                }

                _destructor
            }

            fn generate_pointers() -> ruwren::ClassObjectPointers
            where
                Self: Sized,
            {
                ruwren::ClassObjectPointers {
                    function_pointers: vec![
                        #(
                            #function_decls
                        ),*
                    ]
                }
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
                #constructor_call
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
