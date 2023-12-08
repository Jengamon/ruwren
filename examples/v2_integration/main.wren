import "foobar" for Foo

var f = Foo.new(4)
System.print(Foo.static_fn(f.instance()))
System.print(Foo.sbar)
System.print(f)
f.bar = 42