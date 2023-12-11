import "foobar" for Foo, Teller, Storage

var f = Foo.new(4)
System.print(Foo.static_fn(f.instance(), f))
System.print(Foo.sbar)
System.print(f)
Foo.static_fn(38, null)
System.print(Foo.sbar)
f.bar = "42"
f.bar = 42

System.print(Teller.tell_foo(Storage.foo))
Storage.foo = f
System.print(Teller.tell_foo(Storage.foo))
var t = Teller.new()
System.print(t.teller(t))