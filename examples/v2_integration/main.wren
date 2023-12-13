import "foobar" for Foo

var f = Foo.new(4)
System.print(Foo.static_fn(f.instance(), f))
System.print(Foo.sbar)
System.print(f)
Foo.static_fn(38, null)
System.print(Foo.sbar)
f.bar = "42"
f.bar = 42
