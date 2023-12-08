use std::collections::HashMap;

use crate::{SlotId, VM};

pub trait WrenAtom {
    fn to_vm(self, vm: &VM, slot: SlotId);
    fn from_vm(vm: &VM, slot: SlotId) -> Option<Self>
    where
        Self: Sized;
}

pub trait WrenTo {
    /// VM reserves (1 + SCRATCH_SPACE) when converting
    ///
    /// For example, if ScratchSpace == 1, then conversion functions
    /// can use `slot` and `slot + 1` in its implementation
    const SCRATCH_SPACE: usize = 0;
    fn to_vm(self, vm: &VM, slot: SlotId);
}

pub trait WrenFrom: Sized {
    /// VM reserves (1 + SCRATCH_SPACE) when converting
    ///
    /// For example, if ScratchSpace == 1, then conversion functions
    /// can use `slot` and `slot + 1` in its implementation
    const SCRATCH_SPACE: usize = 0;
    fn from_vm(vm: &VM, slot: SlotId) -> Option<Self>;
}

impl<T: WrenAtom> WrenTo for T {
    fn to_vm(self, vm: &VM, slot: SlotId) {
        <Self as WrenAtom>::to_vm(self, vm, slot)
    }
}

impl<T: WrenAtom> WrenFrom for T {
    fn from_vm(vm: &VM, slot: SlotId) -> Option<Self>
    where
        Self: Sized,
    {
        <Self as WrenAtom>::from_vm(vm, slot)
    }
}

macro_rules! wren_convert {
    (numeric $($ty:ty),+) => {

            $(
                impl WrenAtom for $ty {
                    fn to_vm(self, vm: &VM, slot: SlotId) {
                        vm.set_slot_double(slot, self as f64)
                    }

                    fn from_vm(vm: &VM, slot: SlotId) -> Option<Self> {
                        vm.get_slot_double(slot).map(|i| i as $ty)
                    }
                }
            )+

    };
}

wren_convert!(numeric i8,i16,i32,i64,u8,u16,u32,u64,f32,f64);

impl<const N: usize, T> WrenTo for [T; N]
where
    T: WrenAtom,
{
    const SCRATCH_SPACE: usize = 1;

    fn to_vm(self, vm: &VM, slot: SlotId) {
        vm.set_slot_new_list(slot);
        for (idx, i) in self.into_iter().enumerate() {
            i.to_vm(vm, slot + 1);
            vm.set_list_element(slot, idx as i32, slot + 1);
        }
    }
}

impl<const N: usize, T> WrenFrom for [T; N]
where
    T: WrenAtom,
{
    const SCRATCH_SPACE: usize = 1;

    fn from_vm(vm: &VM, slot: SlotId) -> Option<Self>
    where
        Self: Sized,
    {
        // Taken from https://stackoverflow.com/questions/75310077/array-of-optiont-to-option-array
        fn convert<T, const N: usize>(arr: [Option<T>; N]) -> Option<[T; N]> {
            let arr = arr.into_iter().collect::<Option<Vec<T>>>()?;
            Some(
                arr.try_into()
                    .unwrap_or_else(|_| panic!("the array is of size {N}")),
            )
        }

        let mut items: [Option<T>; N] = std::array::from_fn(|_| None);
        for (i, item) in items.iter_mut().enumerate() {
            vm.get_list_element(slot, i as i32, slot + 1);
            *item = T::from_vm(vm, slot + 1);
        }
        convert(items)
    }
}

impl<T> WrenTo for Vec<T>
where
    T: WrenAtom,
{
    const SCRATCH_SPACE: usize = 1;

    fn to_vm(self, vm: &VM, slot: SlotId) {
        vm.set_slot_new_list(slot);
        for (idx, i) in self.into_iter().enumerate() {
            i.to_vm(vm, slot + 1);
            vm.set_list_element(slot, idx as i32, slot + 1);
        }
    }
}

impl<T> WrenFrom for Vec<T>
where
    T: WrenAtom,
{
    const SCRATCH_SPACE: usize = 1;

    fn from_vm(vm: &VM, slot: SlotId) -> Option<Self>
    where
        Self: Sized,
    {
        let mut items = vec![];
        let count = vm.get_list_count(slot)?;
        for i in 0..count {
            vm.get_list_element(slot, i as i32, slot + 1);
            items.push(T::from_vm(vm, slot + 1));
        }
        items.into_iter().collect()
    }
}

impl<K, V> WrenTo for HashMap<K, V>
where
    K: WrenAtom,
    V: WrenAtom,
{
    const SCRATCH_SPACE: usize = 2;

    fn to_vm(self, vm: &VM, slot: SlotId) {
        vm.set_slot_new_map(slot);
        for (k, v) in self.into_iter() {
            k.to_vm(vm, slot + 1);
            v.to_vm(vm, slot + 2);
            vm.set_map_value(slot, slot + 1, slot + 2);
        }
    }
}
