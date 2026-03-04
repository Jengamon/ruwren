mod convert;

use alloc::{boxed::Box, rc::Rc, string::String, vec, vec::Vec};
use core::{
    any::{Any, TypeId},
    cell::RefCell,
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

impl<T> Slottable<Option<T>> for Option<T>
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
        if slot >= vm.get_slot_count() {
            return None;
        }

        Some(T::try_from_vm(vm, slot, scratch_start))
    }
}

impl<T> Slottable<Vec<Option<T::Source>>> for T
where
    T: Slottable<T::Source, Context = T::Class> + ForeignItem,
{
    type Context = T::Class;

    fn scratch_size() -> usize
    where
        Self: Sized,
    {
        T::scratch_size() + 1
    }

    fn get(
        ctx: &mut Self::Context, vm: &VM, slot: SlotId, scratch_start: SlotId,
    ) -> Option<Vec<Option<T::Source>>> {
        let count = vm.get_list_count(slot)?;
        let mut items = vec![];
        for i in 0..count {
            vm.get_list_element(slot, i as i32, scratch_start);
            items.push(T::get(ctx, vm, scratch_start, scratch_start + 1));
        }
        Some(items)
    }
}

impl<T> Slottable<Option<T::Source>> for T
where
    T: Slottable<T::Source, Context = T::Class> + ForeignItem + 'static,
    T::Class: 'static,
{
    type Context = T::Class;

    fn scratch_size() -> usize
    where
        Self: Sized,
    {
        T::scratch_size()
    }

    fn get(
        ctx: &mut Self::Context, vm: &VM, slot: SlotId, scratch_start: SlotId,
    ) -> Option<Option<T::Source>> {
        T::get_unknown_context(ctx, vm, slot, scratch_start).or_else(|| {
            vm.use_class_mut::<T, _, _>(|vm, cls| {
                cls.map(|class| T::get(class, vm, slot, scratch_start))
            })
        })
    }
}

#[derive(Debug)]
pub struct InputSlot {
    slot: SlotId,
    scratch_start: usize,
    scratch_size: usize,
}

impl InputSlot {
    pub fn new<O: Slottable<O>>(slot: SlotId, arity: usize) -> Self {
        InputSlot {
            slot,
            scratch_start: arity + 1,
            scratch_size: O::scratch_size(),
        }
    }

    pub fn next<O: Slottable<O>>(slot: SlotId, prev: &InputSlot) -> Self {
        InputSlot {
            slot,
            scratch_start: prev.scratch_start + prev.scratch_size,
            scratch_size: O::scratch_size(),
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

pub fn get_slot_value<O>(vm: &VM, slot: &InputSlot, scratch_offset: usize) -> Option<O>
where
    O: Slottable<O, Context = ()>,
{
    O::get(&mut (), vm, slot.slot, scratch_offset + slot.scratch_start)
}

pub fn get_slot_object<T, O>(
    vm: &VM, slot: &InputSlot, scratch_offset: usize, ctx: &mut dyn Any,
) -> Option<O>
where
    T: ForeignItem + Slottable<O, Context = T::Class>,
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
    fn allocate() -> Self;
}

pub trait ForeignItem {
    type Class: V2Class;
    type Source: for<'a> From<(&'a Self::Class, &'a Self)>;

    fn construct(class: &mut Self::Class, vm: &VM) -> Result<Self, String>
    where
        Self: Sized;

    fn create(vm: &VM) -> Result<Self, String>
    where
        Self: Sized + 'static,
    {
        vm.use_class_mut::<Self, _, _>(|vm, cls| cls.map(|class| Self::construct(class, vm)))
            .unwrap_or_else(|| {
                let mut class = Self::Class::allocate();
                let inst = Self::construct(&mut class, vm);
                vm.classes_v2.borrow_mut().insert(
                    TypeId::of::<Self>(),
                    Rc::new(RefCell::new(Box::new(class) as Box<Self::Class>)),
                );
                inst
            })
    }
}

impl<T> Class for T
where
    T: ForeignItem + 'static,
{
    fn initialize(_vm: &VM) -> Self
    where
        Self: Sized,
    {
        unreachable!("This is only implemented for compatibility, and should never be called for v2 foreigns")
    }
}
