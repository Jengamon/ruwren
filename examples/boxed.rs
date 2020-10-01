use ruwren::{VMConfig, VM, Class, ModuleLibrary, FunctionSignature, create_module, get_slot_checked, send_foreign};

struct BoxxWrapper(Box<dyn Boxx>);

impl Class for BoxxWrapper {
  fn initialize(_: &VM) -> BoxxWrapper {
    unreachable!()
  }
}

impl BoxxWrapper {
  fn flipp(&self, vm: &VM) {
    vm.set_slot_double(0, self.0.flipp() as f64);
  }
}

trait Boxx {
  fn flipp(&self) -> u32;
}

impl Class for Box<dyn Boxx> {
  fn initialize(_: &VM) -> Self {
      panic!("Can't initialize DynBoxx")
  }
}

#[derive(Clone)]
struct Bonafide {
  x: u32,
  y: u32
}

impl Class for Bonafide {
  fn initialize(vm: &VM) -> Bonafide {
    let x = get_slot_checked!(vm => num 1) as u32;
    let y = get_slot_checked!(vm => num 2) as u32;
    Bonafide {x, y}
  }
}

impl Bonafide {
  fn send_box(&self, vm: &VM) {
    send_foreign!(vm, "boxed", "DynBoxx", Box::new(self.clone()) as Box<dyn Boxx> => 0);
  }

  fn send_wrapped(&self, vm: &VM) {
    send_foreign!(vm, "boxed", "DynBoxxWrapped", BoxxWrapper(Box::new(self.clone())) => 0);
  }
}

impl Boxx for Bonafide {
  fn flipp(&self) -> u32 { self.x + self.y }
}

create_module! {
  class("Bonafide") crate::Bonafide => bonafide {
    instance(getter "box") send_box,
    instance(getter "wrap") send_wrapped
  }

  class("DynBoxx") Box<dyn crate::Boxx> => boxx {
  }

  class("DynBoxxWrapped") crate::BoxxWrapper => boxxwrap {
    instance(getter "flipp") flipp
  }

  module => boxed
}

const BONAFIDE_MODULE_SRC: &'static str = include_str!("boxed/bonafide.wren");

fn main() {
  let mut lib = ModuleLibrary::new();
  boxed::publish_module(&mut lib);
  let vm = VMConfig::new().library(&lib).build();
  vm.interpret("boxed", BONAFIDE_MODULE_SRC).unwrap(); // Should succeed

  let res = vm.interpret("main", include_str!("boxed/main.wren"));
  if let Err(err) = res {
      eprintln!("Load error: {}", err);
      return;
  }

  vm.execute(|vm| {
    vm.get_variable("main", "BoxedTest", 0);
  });

  let res = vm.call(FunctionSignature::new_getter("main"));

  if let Err(err) = res {
      eprintln!("{}", err);
  } else {
    vm.execute(|vm| {
      let f = get_slot_checked!(vm => foreign Box<dyn crate::Boxx> => 0);
      println!("boxed> {}", f.flipp());
    })
  }
}