import munit.FunSuite
import scala.collection.mutable
class MainSuite extends FunSuite {
  var f = Func5(Set(Func5i(10, 1, 3))) // accepts [1, 4]
  test(s"$f([3, 5)) == {[12, 13)}") {
    val want = Set((12, 13), (4,5))
    val got = f(3L, 5L)
    assert(want == got, s"Wanted $want but got $got")
  }
  test(s"$f((1, 9)) = ${Set((10, 13), (22, 24), (8, 9))}") {
    val f =
      Func5(Set(Func5i(10, 1, 3), Func5i(22, 5, 2))) // accepts [1, 3], [5, 7]
    val want = Set((10, 13), (22, 24), (8, 9))
    val got = f(1L, 9L)
    assert(want == got, s"Wanted $want but got $got")
  }
  val f1 = Func5i(-1, 3, 5)
  test("Funci {[3, 8] -> ? } accepts (3, 5) == Some(3, 5)") {
    assert(f1.accepts((3L, 5L)) == Some(3, 5))
  }
  test("Funci {[3, 8] -> ? } accepts (4, 5) == Some(4, 5)") {
    assert(f1.accepts((4L, 5L)) == Some(4, 5))
  }
  test("Funci {[3, 8] -> ? } accepts (4, 10) == Some(4, 8)") {
    assert(f1.accepts((4L, 10L)) == Some(4, 8))
  }
  test("Funci {[3, 8] -> ? } accepts (2, 9) == Some(3, 8)") {
    assert(f1.accepts((2L, 9L)) == Some(3, 8))
  }
  test("Funci {[3, 8) -> ? } accepts [8, 9) == Some(8, 8)") {
    assert(f1.accepts((8L, 9L)) == None)
  }
  test("Funci {[3, 8] -> ? } accepts (9, 10) == None") {
    assert(f1.accepts((9L, 10L)) == None)
  }
  test("Funci {[3, 8] -> ? } accepts (1, 2) == None") {
    assert(f1.accepts((1L, 2L)) == None)
  }
  test("Func5i {[1, 3] -> [10, 13], [5, 7] -> [22, 24]}") {
    val f =
      Func5(Set(Func5i(10, 1, 3), Func5i(22, 5, 2))) // accepts [1, 3], [5, 7]
    val want = Set((10, 13), (22, 24), (8, 9))
    val got = f(1L, 9L)
    assert(want == got, s"Wanted $want but got $got")
  }
  test("5 part 2 sample input") {
    val out1 = solve5Part2("./inputs/day5_input.txt")
    assert(out1 == 46, s"example failed! wanted 46 but got $out1")
  }
  test("Here") {
    val f = Func5.fromChunk("""fetrilizer=to-water map
      |49 53 8
      |0 11 42
      |42 0 7
      |57 7 4""".stripMargin)
    val want = Set((53L, 57L), (62L, 70L), (81L, 95L))
    println(f)
    val got = f(Set((57L, 70L), (81L, 95L)))
    assert(want == got, s"Wanted $want but got $got")
  }
}
