import "boxed" for Bonafide

class BoxedTest {
  static main {
    var a = Bonafide.new(4, 7)
    return a.boxed
  }
}
