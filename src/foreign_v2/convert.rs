use std::{any::type_name, collections::HashMap, string::FromUtf8Error};

use crate::{SlotId, SlotType, VM};

pub trait WrenAtom {
    const SCRATCH_SPACE: usize = 0;
    fn to_vm(self, vm: &VM, slot: SlotId, scratch_start: SlotId);
    fn from_vm(vm: &VM, slot: SlotId, scratch_start: SlotId) -> Option<Self>
    where
        Self: Sized;
}

pub trait WrenTo {
    /// VM reserves (1 + SCRATCH_SPACE) when converting
    ///
    /// For example, if ScratchSpace == 1, then conversion functions
    /// can use `slot` and `scratch_slot` in its implementation
    const SCRATCH_SPACE: usize = 0;
    fn to_vm(self, vm: &VM, slot: SlotId, scratch_start: SlotId);
}

impl<D, E> WrenTo for Result<D, E>
where
    D: WrenTo,
    E: WrenTo,
{
    const SCRATCH_SPACE: usize = D::SCRATCH_SPACE + E::SCRATCH_SPACE;
    fn to_vm(self, vm: &VM, slot: SlotId, scratch_start: SlotId) {
        match self {
            Ok(d) => D::to_vm(d, vm, slot, scratch_start),
            Err(e) => {
                E::to_vm(e, vm, slot, scratch_start + D::SCRATCH_SPACE);
                vm.abort_fiber(slot);
            }
        }
    }
}

pub trait WrenFrom: Sized {
    /// VM reserves (1 + SCRATCH_SPACE) when converting
    ///
    /// For example, if ScratchSpace == 1, then conversion functions
    /// can use `slot` and `scratch_slot` in its implementation
    const SCRATCH_SPACE: usize = 0;
    /// Note: This should be *infallible*, so it's rare to have this directly implemented
    fn from_vm(vm: &VM, slot: SlotId, scratch_start: SlotId) -> Self;
}

pub trait WrenTryFrom: Sized {
    /// VM reserves (1 + SCRATCH_SPACE) when converting
    ///
    /// For example, if ScratchSpace == 1, then conversion functions
    /// can use `slot` and `slot + 1` in its implementation
    const SCRATCH_SPACE: usize = 0;
    fn try_from_vm(vm: &VM, slot: SlotId, scratch_start: SlotId) -> Option<Self>;
}

impl<T: WrenAtom> WrenTo for T {
    const SCRATCH_SPACE: usize = <T as WrenAtom>::SCRATCH_SPACE;
    fn to_vm(self, vm: &VM, slot: SlotId, scratch_start: SlotId) {
        <Self as WrenAtom>::to_vm(self, vm, slot, scratch_start)
    }
}

impl<T: WrenAtom> WrenTryFrom for T {
    const SCRATCH_SPACE: usize = <T as WrenAtom>::SCRATCH_SPACE;
    fn try_from_vm(vm: &VM, slot: SlotId, scratch_start: SlotId) -> Option<Self>
    where
        Self: Sized,
    {
        if slot >= vm.get_slot_count() {
            return None;
        }

        <Self as WrenAtom>::from_vm(vm, slot, scratch_start)
    }
}

impl<T: WrenTryFrom> WrenFrom for T {
    const SCRATCH_SPACE: usize = <T as WrenTryFrom>::SCRATCH_SPACE;
    fn from_vm(vm: &VM, slot: SlotId, scratch_start: SlotId) -> Self {
        T::try_from_vm(vm, slot, scratch_start)
            .unwrap_or_else(|| panic!("expected slot {} to be type {}", slot, type_name::<T>()))
    }
}

macro_rules! wren_convert {
    (numeric $($ty:ty),+) => {
            $(
                impl WrenAtom for $ty {
                    fn to_vm(self, vm: &VM, slot: SlotId, _scratch_start: SlotId) {
                        vm.set_slot_double(slot, self as f64)
                    }

                    fn from_vm(vm: &VM, slot: SlotId, _scratch_start: SlotId) -> Option<Self> {
                        if vm.get_slot_type(slot) == SlotType::Num {
                            vm.get_slot_double(slot).map(|i| i as $ty)
                        } else {
                            None
                        }
                    }
                }
            )+

    };
}

impl WrenAtom for () {
    fn to_vm(self, vm: &VM, slot: SlotId, _scratch_start: SlotId) {
        vm.set_slot_null(slot)
    }

    fn from_vm(_vm: &VM, _slot: SlotId, _scratch_start: SlotId) -> Option<Self>
    where
        Self: Sized,
    {
        Some(())
    }
}

impl WrenAtom for bool {
    fn to_vm(self, vm: &VM, slot: SlotId, _scratch_start: SlotId) {
        vm.set_slot_bool(slot, self)
    }

    fn from_vm(vm: &VM, slot: SlotId, _scratch_start: SlotId) -> Option<Self>
    where
        Self: Sized,
    {
        if vm.get_slot_type(slot) == SlotType::Bool {
            vm.get_slot_bool(slot)
        } else {
            None
        }
    }
}

// Wren strings aren't guaranteed to be UTF-8, so to get a string,
// accept a WrenString, then call its `into_string`
impl WrenTo for String {
    fn to_vm(self, vm: &VM, slot: SlotId, _scratch_start: SlotId) {
        vm.set_slot_string(slot, self)
    }
}

wren_convert!(numeric i8,i16,i32,i64,u8,u16,u32,u64,f32,f64);

pub struct WrenString(Vec<u8>);

impl WrenString {
    pub fn bytes(&self) -> &[u8] {
        &self.0
    }

    pub fn bytes_mut(&mut self) -> &mut Vec<u8> {
        &mut self.0
    }

    pub fn into_string(self) -> Result<String, FromUtf8Error> {
        String::from_utf8(self.0)
    }
}

impl WrenAtom for WrenString {
    fn to_vm(self, vm: &VM, slot: SlotId, _scratch_start: SlotId) {
        vm.set_slot_bytes(slot, &self.0)
    }

    fn from_vm(vm: &VM, slot: SlotId, _scratch_start: SlotId) -> Option<Self>
    where
        Self: Sized,
    {
        if vm.get_slot_type(slot) == SlotType::String {
            vm.get_slot_bytes(slot).map(WrenString)
        } else {
            None
        }
    }
}

pub enum WrenValue {
    Null,
    Number(f64),
    String(Vec<u8>),
    Bool(bool),
}

impl WrenAtom for WrenValue {
    fn to_vm(self, vm: &VM, slot: SlotId, _scratch_start: SlotId) {
        match self {
            Self::Number(val) => vm.set_slot_double(slot, val),
            Self::String(string) => vm.set_slot_bytes(slot, &string),
            Self::Bool(val) => vm.set_slot_bool(slot, val),
            Self::Null => vm.set_slot_null(slot),
        }
    }

    fn from_vm(vm: &VM, slot: SlotId, _scratch_start: SlotId) -> Option<Self>
    where
        Self: Sized,
    {
        match vm.get_slot_type(slot) {
            SlotType::Num => vm.get_slot_double(slot).map(Self::Number),
            SlotType::String => vm.get_slot_bytes(slot).map(Self::String),
            SlotType::Bool => vm.get_slot_bool(slot).map(Self::Bool),
            SlotType::Null => Some(Self::Null),
            _ => None, // Any other types are not supported by the dynamic value API
        }
    }
}

impl<T> WrenTo for Option<T>
where
    T: WrenTo,
{
    const SCRATCH_SPACE: usize = T::SCRATCH_SPACE;
    fn to_vm(self, vm: &VM, slot: SlotId, scratch_start: SlotId) {
        match self {
            Some(t) => T::to_vm(t, vm, slot, scratch_start),
            None => vm.set_slot_null(slot),
        }
    }
}

impl<const N: usize, T> WrenTo for [T; N]
where
    T: WrenTo,
{
    const SCRATCH_SPACE: usize = 1 + T::SCRATCH_SPACE;

    fn to_vm(self, vm: &VM, slot: SlotId, scratch_start: SlotId) {
        vm.set_slot_new_list(slot);
        for (idx, i) in self.into_iter().enumerate() {
            i.to_vm(vm, scratch_start, scratch_start + 1);
            vm.set_list_element(slot, idx as i32, scratch_start);
        }
    }
}

impl<const N: usize, T> WrenTryFrom for [T; N]
where
    T: WrenTryFrom,
{
    const SCRATCH_SPACE: usize = 1 + T::SCRATCH_SPACE;

    fn try_from_vm(vm: &VM, slot: SlotId, scratch_start: SlotId) -> Option<Self>
    where
        Self: Sized,
    {
        if slot <= vm.get_slot_count() || vm.get_slot_type(slot) != SlotType::List {
            return None;
        }

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
            vm.get_list_element(slot, i as i32, scratch_start);
            *item = T::try_from_vm(vm, scratch_start, scratch_start + 1);
        }
        convert(items)
    }
}

impl<T> WrenTo for Vec<T>
where
    T: WrenTo,
{
    const SCRATCH_SPACE: usize = 1 + T::SCRATCH_SPACE;

    fn to_vm(self, vm: &VM, slot: SlotId, scratch_start: SlotId) {
        vm.set_slot_new_list(slot);
        for (idx, i) in self.into_iter().enumerate() {
            i.to_vm(vm, scratch_start, scratch_start + 1);
            vm.insert_in_list(slot, idx as i32, scratch_start);
        }
    }
}

impl<T> WrenTryFrom for Vec<T>
where
    T: WrenTryFrom,
{
    const SCRATCH_SPACE: usize = 1 + T::SCRATCH_SPACE;

    fn try_from_vm(vm: &VM, slot: SlotId, scratch_start: SlotId) -> Option<Self>
    where
        Self: Sized,
    {
        if slot <= vm.get_slot_count() || vm.get_slot_type(slot) != SlotType::List {
            return None;
        }

        let mut items = vec![];
        let count = vm.get_list_count(slot)?;
        for i in 0..count {
            vm.get_list_element(slot, i as i32, scratch_start);
            items.push(T::try_from_vm(vm, scratch_start, scratch_start + 1)?);
        }
        Some(items)
    }
}

impl<K, V> WrenTo for HashMap<K, V>
where
    K: WrenAtom,
    V: WrenAtom,
{
    const SCRATCH_SPACE: usize = 2 + K::SCRATCH_SPACE + V::SCRATCH_SPACE;

    fn to_vm(self, vm: &VM, slot: SlotId, scratch_start: SlotId) {
        vm.set_slot_new_map(slot);
        for (k, v) in self.into_iter() {
            k.to_vm(vm, scratch_start, scratch_start + 2);
            v.to_vm(vm, scratch_start + 1, scratch_start + 2);
            vm.set_map_value(slot, scratch_start, scratch_start + 1);
        }
    }
}
