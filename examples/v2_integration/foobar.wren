foreign class Foo {
    construct new(bar) {}
    foreign bar=(val)
    foreign static sbar
    foreign instance()
    foreign static static_fn(num, foo)

    toString { super.toString + " { bar = %(this.instance()), sbar = %(type.sbar) }" }
}
