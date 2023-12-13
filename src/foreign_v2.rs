mod convert;

use std::{
    any::{type_name, Any, TypeId},
    cell::RefCell,
    rc::Rc,
};

pub use convert::*;

use crate::{Class, SlotId, VM};

/// Produce O given context Self::Context?
pub trait Slottable<O> {
    type Context;
    fn scratch_size() -> usize
    where
        Self: Sized;
    fn get(ctx: &mut Self::Context, vm: &VM, slot: SlotId, scratch_start: SlotId) -> Option<O>;

    fn get_unknown_context(
        ctx: &mut dyn Any, vm: &VM, slot: SlotId, scratch_start: SlotId,
    ) -> Option<Option<O>>
    where
        Self::Context: 'static,
    {
        ctx.downcast_mut()
            .map(|ctx| Self::get(ctx, vm, slot, scratch_start))
    }
}

impl<T> Slottable<T> for T
where
    T: WrenTryFrom,
{
    type Context = ();

    fn scratch_size() -> usize {
        T::SCRATCH_SPACE
    }

    fn get(_ctx: &mut (), vm: &VM, slot: SlotId, scratch_start: SlotId) -> Option<Self>
    where
        Self: Sized,
    {
        T::try_from_vm(vm, slot, scratch_start)
    }
}

#[derive(Debug)]
pub struct InputSlot {
    slot: SlotId,
    scratch_start: usize,
    scratch_size: usize,
}

impl InputSlot {
    pub fn new<O, T: Slottable<O>>(slot: SlotId, arity: usize) -> Self {
        InputSlot {
            slot,
            scratch_start: arity + 1,
            scratch_size: T::scratch_size(),
        }
    }

    pub fn next<O, T: Slottable<O>>(slot: SlotId, prev: &InputSlot) -> Self {
        InputSlot {
            slot,
            scratch_start: prev.scratch_start + prev.scratch_size,
            scratch_size: T::scratch_size(),
        }
    }

    pub fn object_new(slot: SlotId, arity: usize) -> Self {
        InputSlot {
            slot,
            scratch_start: arity + 1,
            scratch_size: 1,
        }
    }

    pub fn object_next(slot: SlotId, prev: &InputSlot) -> Self {
        InputSlot {
            slot,
            scratch_start: prev.scratch_start + prev.scratch_size,
            scratch_size: 1,
        }
    }

    pub fn scratch_end(&self) -> usize {
        self.scratch_start + self.scratch_size
    }
}

pub fn get_slot_value<O>(vm: &VM, slot: &InputSlot, scratch_offset: usize) -> O
where
    O: Slottable<O, Context = ()>,
{
    O::get(&mut (), vm, slot.slot, scratch_offset + slot.scratch_start)
        .unwrap_or_else(|| panic! {"slot {} cannot be type {}", slot.slot, type_name::<O>()})
}

pub fn get_slot_object<T>(
    vm: &VM, slot: &InputSlot, scratch_offset: usize, ctx: &mut dyn Any,
) -> Option<T::Source>
where
    T: ForeignItem + Slottable<T::Source, Context = T::Class>,
    T::Class: 'static,
    T: 'static,
{
    match T::get_unknown_context(ctx, vm, slot.slot, scratch_offset + slot.scratch_start) {
        None => vm.use_class_mut::<T, _, _>(|vm, cls| {
            cls.and_then(|class| T::get(class, vm, slot.slot, slot.scratch_start))
        }),
        Some(obj) => obj,
    }
}

pub trait V2Class {
    fn name() -> &'static str;
}

pub trait ForeignItem {
    type Class: V2Class + V2ClassAllocator;
    type Source: for<'a> From<(&'a Self::Class, &'a Self)>;

    fn construct(class: &mut Self::Class, vm: &VM) -> Self;
}

pub trait V2ClassAllocator: V2Class {
    fn allocate() -> Self;
}

impl<T> Class for T
where
    T: ForeignItem + 'static,
{
    fn initialize(vm: &VM) -> Self
    where
        Self: Sized,
    {
        vm.use_class_mut::<Self, _, _>(|vm, cls| cls.map(|class| T::construct(class, vm)))
            .unwrap_or_else(|| {
                let mut class = T::Class::allocate();
                let inst = T::construct(&mut class, vm);
                vm.classes_v2.borrow_mut().insert(
                    TypeId::of::<T>(),
                    Rc::new(RefCell::new(Box::new(class) as Box<T::Class>)),
                );
                inst
            })
    }
}
