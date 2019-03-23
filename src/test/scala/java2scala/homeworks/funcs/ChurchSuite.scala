package java2scala.homeworks.funcs

import org.scalactic.Equality
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec, WordSpec}

import scala.Function.uncurried

class ChurchSuite extends WordSpec with Matchers with PropertyChecks {
  "ChurchBool" when {
    "converting to and from boolean" should {
      "be isomorphic" in forAll { b: Boolean =>
        ChurchBool(b).toBool shouldEqual b
      }

      "convert True to true" in {
        ChurchBool.True.toBool shouldEqual true
      }

      "convert False to false" in {
        ChurchBool.False.toBool shouldEqual false
      }
    }

    "checking conditions" should {
      "use first expr for true" in forAll { (x: Int, y: Int) =>
        ChurchBool.True.cif(x)(y) shouldEqual x
      }

      "use second expr for false" in forAll { (x: Int, y: Int) =>
        ChurchBool.False.cif(x)(y) shouldEqual y
      }
    }

    "negating" should {
      "negate converted to the negated" in forAll { b: Boolean =>
        (!ChurchBool(b)).toBool shouldEqual !b
      }

      "double negate to the same" in forAll { b: Boolean =>
        (!(!ChurchBool(b))).toBool shouldEqual b
      }
    }

    "calc AND" should {
      "keep and" in forAll { (b: Boolean, c: Boolean) =>
        (ChurchBool(b) && ChurchBool(c)).toBool shouldEqual b && c
      }

      "true neutral" in forAll { b: Boolean =>
        (ChurchBool(b) && ChurchBool.True).toBool shouldEqual b
        (ChurchBool.True && ChurchBool(b)).toBool shouldEqual b
      }

      "false absorb" in forAll { b: Boolean =>
        (ChurchBool(b) && ChurchBool.False).toBool shouldEqual false
        (ChurchBool.False && ChurchBool(b)).toBool shouldEqual false
      }
    }

    "calc OR" should {
      "keep OR" in forAll { (b: Boolean, c: Boolean) =>
        (ChurchBool(b) || ChurchBool(c)).toBool shouldEqual b || c
      }

      "true absorb" in forAll { b: Boolean =>
        (ChurchBool(b) || ChurchBool.True).toBool shouldEqual true
        (ChurchBool.True || ChurchBool(b)).toBool shouldEqual true
      }

      "false neutral" in forAll { b: Boolean =>
        (ChurchBool(b) || ChurchBool.False).toBool shouldEqual b
        (ChurchBool.False || ChurchBool(b)).toBool shouldEqual b
      }
    }
  }

  "Church List" when {
    "folding" should {
      "be compatible with foldRight" in forAll { (list: List[Int], z: Long, c: Int => Long => Long) =>
        ChurchList(list: _*).fold(z)(c)
      }

      "preserve list structure" in forAll { list: List[Int] =>
        ChurchList(list: _*).toList shouldEqual list
      }

      "preserve vector structure" in forAll { vec: Vector[Int] =>
        ChurchList(vec: _*).toList.toVector shouldEqual vec
      }
    }

    "mapping" should {
      "be compatible with collection map" in forAll { (list: Array[Int], f: Int => Long, z: Long, c: Long => Long => Long) =>
        ChurchList(list: _*).map(f).toList shouldEqual list.map(f)
      }
      "keep composition" in forAll { (list: List[Int], f: Int => String, g: String => Long) =>
        ChurchList(list: _*).map(f).map(g) shouldEqual ChurchList(list: _*).map(f andThen g)
      }

    }

    "filtering" should {
      "be compatible with collection filter" in forAll { (list: List[Int], f: Int => Boolean) =>
        ChurchList(list: _*).filter(f).toList shouldEqual list.filter(f)
      }

      "keep and" in forAll { (list: List[Int], f: Int => Boolean, g: Int => Boolean) =>
        ChurchList(list: _*).filter(f).filter(g) shouldEqual
          ChurchList(list: _*).filter(x => f(x) && g(x))
      }
    }

    "head option" should {
      "return None for empty" in {
        ChurchList.empty[Int].headOption shouldEqual None
      }

      "return first element for concatenation" in forAll { (x: Int, xs: List[Int]) =>
        (x :: ChurchList(xs: _*)).headOption shouldEqual Some(x)
      }
    }

    "flatMap" should {
      "be compatible with list flatMap" in forAll { (l: List[Int], f: Int => List[Long]) =>
        ChurchList(l: _*).flatMap(l => ChurchList(f(l): _*)).toList shouldEqual l.flatMap(f)

      }

      "return zero for empty" in {
        ChurchList.empty[Int].flatMap(_ => ChurchList(1)).headOption shouldEqual None
      }

      "return zero for empty map " in {
        ChurchList(1).flatMap(_ => ChurchList.empty[Int]).headOption shouldEqual None
      }

      "respect left unit rule" in forAll { (x: Int, f: Int => ChurchList[Long]) =>
        ChurchList(x).flatMap(f) shouldEqual f(x)
      }

      "respect right unit rule" in forAll { xs: ChurchList[Int] =>
        xs.flatMap(x => ChurchList(x)) shouldEqual xs
      }

      "respect associativity rule" in forAll { (xs: ChurchList[Int], f: Int => ChurchList[Long], g: Long => ChurchList[Int]) =>
        xs.flatMap(f).flatMap(g) shouldEqual xs.flatMap(l => f(l).flatMap(g))
      }

      "be equal to map" in forAll { (xs: ChurchList[Int], f: Int => Long) =>
        xs.flatMap(x => ChurchList(f(x))) shouldEqual xs.map(f)
      }
    }

    "concatetation" should {
      "be compatible with list concatenation" in { (xs: List[Long], ys: List[Long]) =>
        (ChurchList(xs: _*) ++ ChurchList(ys: _*)).toList shouldEqual xs ++ ys
      }

      "distribute with map" in { (xs: ChurchList[Long], ys: ChurchList[Long], f: Long => Int) =>
        xs.map(f) ++ ys.map(f) shouldEqual (xs ++ ys).map(f)
      }

      "distribute with filter" in { (xs: ChurchList[Long], ys: ChurchList[Long], f: Long => Boolean) =>
        xs.filter(f) ++ ys.filter(f) shouldEqual (xs ++ ys).filter(f)
      }
    }

    "drop" should {
      "be compatible with collection drop" in forAll { (xs: List[Long], dropCount: Int) =>
        ChurchList(xs: _*).drop(dropCount).toList shouldEqual xs.drop(dropCount)
      }

      "be compatible with collection drop, when drop count is negative" in forAll { (xs: List[Long], dropCount: Int) =>
        whenever(dropCount < 0) {
          ChurchList(xs: _*).drop(dropCount).toList shouldEqual xs.drop(dropCount)
        }
      }

      "be compatible with collection drop, when drop count is more than list length" in forAll { (xs: List[Long], dropCount: Int) =>
        whenever(dropCount > xs.length) {
          ChurchList(xs: _*).drop(dropCount).toList shouldEqual xs.drop(dropCount)
        }
      }

      "return empty for empty" in { dropCount: Int =>
        ChurchList.empty[Int].drop(dropCount) shouldEqual ChurchList.empty[Int]
      }
    }
  }
}
