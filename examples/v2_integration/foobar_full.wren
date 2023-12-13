foreign class Foo {
    construct new(b) {}
    foreign bar=(val)
    foreign static sbar
    foreign instance()
    foreign static static_fn(num, foo)

    toString { super.toString + " { bar = %(this.instance()), sbar = %(type.sbar) }" }
}

foreign class Teller {
    construct new() {}
    foreign static tell_foo(t)
    foreign teller(teller)
}

foreign class Storage {
    foreign static foo=(foo)
    foreign static foo
}
