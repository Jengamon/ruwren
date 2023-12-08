mod convert;

use std::{any::TypeId, marker::PhantomData};

pub use convert::*;

use crate::{Class, SlotId, VM};

pub struct InputSlot<T> {
    marker: PhantomData<T>,
    offset: SlotId,
}

impl<T> InputSlot<T>
where
    T: WrenFrom,
{
    pub fn first() -> Self {
        InputSlot {
            marker: PhantomData,
            offset: 1,
        }
    }

    pub fn next<O: WrenFrom>(prev: &InputSlot<O>) -> Self {
        InputSlot {
            marker: PhantomData,
            offset: prev.slot_end(),
        }
    }

    pub fn slot(&self) -> SlotId {
        self.offset
    }

    pub fn slot_end(&self) -> SlotId {
        self.offset + 1 + T::SCRATCH_SPACE
    }

    pub fn value(&self, vm: &VM) -> Option<T> {
        T::from_vm(vm, self.slot())
    }
}

pub trait V2Class {
    fn name() -> &'static str;
}

pub trait ForeignItem {
    type Class: V2Class + V2ClassAllocator;

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
