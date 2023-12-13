import "boxed" for Bonafide

class BoxedTest {
  static main {
    var a = Bonafide.new(4, 7)
    System.print("Coolio> " + a.wrap.flipp.toString)
    System.print("Double coolio> " + a.flipp.toString)
    return a.box
  }
}
