foreign class Vector {
    construct invalid() {}
    foreign x=(x)
    foreign y=(y)

    foreign static read(vecs)

    toString { "<%(this.x) %(this.y)>" }

    foreign x
    foreign y
    foreign copy()
}

class Math {
    foreign static new_vector(x, y)
}
