import munit.FunSuite
import scala.collection.mutable
class MainSuite extends FunSuite {

  test("Hand2 types") {
    def f = CamelPoker.getHandType2
    assert(CamelPoker.HandType.FiveOfAKind == f("JJJJJ"))
    assert(CamelPoker.HandType.FiveOfAKind == f("22222"))
    assert(CamelPoker.HandType.FiveOfAKind == f("222JJ"))
    assert(CamelPoker.HandType.FiveOfAKind == f("2222J"))
    assert(CamelPoker.HandType.FourOfAKind == f("22223"))
    assert(CamelPoker.HandType.FourOfAKind == f("2223J"))
    assert(CamelPoker.HandType.FourOfAKind == f("223JJ"))
    assert(CamelPoker.HandType.FourOfAKind == f("23JJJ"))
    assert(CamelPoker.HandType.FourOfAKind == f("21J11"))
    assert(CamelPoker.HandType.FullHouse == f("22111"))
    assert(CamelPoker.HandType.FullHouse == f("22J11"))
    assert(CamelPoker.HandType.ThreeofAKind == f("22214"))
    assert(CamelPoker.HandType.ThreeofAKind == f("22J14"))
    val got = f("2JJ31")
    assert(CamelPoker.HandType.ThreeofAKind == got, s"Wanted ${CamelPoker.HandType.ThreeofAKind} but got ${got}") // !
    assert(CamelPoker.HandType.TwoPair == f("22331"))
    assert(CamelPoker.HandType.FourOfAKind == f("22JJ1"))
    assert(CamelPoker.HandType.OnePair == f("22314"))
    assert(CamelPoker.HandType.OnePair == f("5J314"))


    def h = CamelPoker.Hand2
    assert(h("33333") < h("22222"))
    assert(h("33333") == h("33333"))
    assert(h("22222") < h("JJJJJ"))
    assert(h("22222") < h("KQJ123"))
    assert(h("2222J") < h("22223"))
    assert(h("KKKKK") < h("22222"))
    assert(h("22222") < h("KKKK3"))
  }

  var f = Func5(Set(Func5i(10, 1, 3))) // accepts [1, 4]
  test(s"$f([3, 5)) == {[12, 13)}") {
    val want = Set((12, 13), (4, 5))
    val got = f(3L, 5L)
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
  test("Here") {
    val f = Func5.fromChunk("""fetrilizer=to-water map
        |49 53 8
        |0 11 42
        |42 0 7
        |57 7 4""".stripMargin)
    val want = Set((53L, 57L), (61L, 70L), (81L, 95L))
    println(f)
    val got = f(Set((57L, 70L), (81L, 95L)))
    assert(want == got, s"Wanted $want but got $got")
  }
}
