mod convert;

use std::{
    any::{type_name, Any, TypeId},
    marker::PhantomData,
};

pub use convert::*;

use crate::{Class, SlotId, VM};

pub trait Slottable<O> {
    type Context;
    fn scratch_size() -> usize
    where
        Self: Sized;
    fn get(ctx: &mut Self::Context, vm: &VM, slot: SlotId, scratch_start: SlotId) -> O;

    fn get_unknown_context(
        ctx: &mut dyn Any, vm: &VM, slot: SlotId, scratch_start: SlotId,
    ) -> Option<O>
    where
        Self::Context: 'static,
    {
        ctx.downcast_mut()
            .and_then(|ctx| Some(Self::get(ctx, vm, slot, scratch_start)))
    }
}

impl<T> Slottable<T> for T
where
    T: WrenFrom,
{
    type Context = ();

    fn scratch_size() -> usize {
        T::SCRATCH_SPACE
    }

    fn get(_ctx: &mut (), vm: &VM, slot: SlotId, scratch_start: SlotId) -> Self
    where
        Self: Sized,
    {
        T::from_vm(vm, slot, scratch_start)
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
}

pub fn get_slot_object<T>(
    vm: &VM, slot: &InputSlot, scratch_offset: usize, ctx: &mut dyn Any,
) -> Option<T::Source>
where
    T: ForeignItem + Slottable<T::Source, Context = T::Class>,
    T::Class: 'static,
    T: 'static,
{
    T::get_unknown_context(ctx, vm, slot.slot, scratch_offset + slot.scratch_start).or_else(|| {
        vm.use_class::<T, _, _>(|vm, cls| {
            cls.and_then(|class| Some(T::get(class, vm, slot.slot, slot.scratch_start)))
        })
    })
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

// impl<T> Extractable<T::Class> for T
// where
//     T: ForeignItem + ClassObject,
//     T: 'static,
// {
//     type Output = T::Source;
//     const SCRATCH_SPACE: usize = 1;
//     fn extract(
//         context: &mut T::Class, vm: &VM, slot: SlotId, _scratch_start: SlotId,
//     ) -> Self::Output {
//         let inst = vm.get_slot_foreign::<T>(slot).expect(&format!(
//             "Item in slot {} is not of type {}",
//             slot,
//             type_name::<T>()
//         ));
//         Into::<T::Source>::into((&*context, inst))
//     }
// }

impl<T> Class for T
where
    T: ForeignItem + 'static,
{
    fn initialize(vm: &VM) -> Self
    where
        Self: Sized,
    {
        let mut classes_v2 = vm.classes_v2.borrow_mut();
        if let Some(cls) = classes_v2
            .get_mut(&TypeId::of::<T>())
            .and_then(|mcls| mcls.downcast_mut::<T::Class>())
        {
            T::construct(cls, vm)
        } else {
            let mut class = T::Class::allocate();
            let inst = T::construct(&mut class, vm);
            classes_v2.insert(TypeId::of::<T>(), Box::new(class) as Box<T::Class>);
            inst
        }
    }
}
