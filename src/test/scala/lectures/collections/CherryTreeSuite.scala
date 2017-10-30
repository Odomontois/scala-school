package lectures.collections

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers, PropSpec}

class CherryTreeSuite extends FlatSpec with PropertyChecks with Matchers {
  "Cherry tree" should "append element" in forAll { (x: Int, xs: Vector[Int]) =>
    val tree = CherryTree(xs: _*)
    tree.append(x) shouldBe CherryTree(xs :+ x: _*)
  }

  it should "prepend element" in forAll { (x: Int, xs: Vector[Int]) =>
    val tree = CherryTree(xs: _*)
    tree.prepend(x) shouldBe CherryTree(x +: xs: _*)
  }

  it should "get tail" in forAll { xs: Vector[Int] =>
    val tree = CherryTree(xs: _*)
    if (xs.isEmpty) an[UnsupportedOperationException] should be thrownBy tree.tail
    else tree.tail shouldBe CherryTree(xs.tail: _*)
  }

  it should "get head" in forAll { xs: Vector[Int] =>
    val tree = CherryTree(xs: _*)
    if (xs.isEmpty) an[NoSuchElementException] should be thrownBy tree.head
    else tree.head shouldBe xs.head
  }

  it should "get init" in forAll { xs: Vector[Int] =>
    val tree = CherryTree(xs: _*)
    if (xs.isEmpty) an[UnsupportedOperationException] should be thrownBy tree.init
    else tree.init shouldBe CherryTree(xs.init: _*)
  }

  it should "get last" in forAll { xs: Vector[Int] =>
    val tree = CherryTree(xs: _*)
    if (xs.isEmpty) an[NoSuchElementException] should be thrownBy tree.last
    else tree.last shouldBe xs.last
  }

  it should "get element by index" in forAll { (xs: Vector[Int], i: Int) =>
    whenever(i < xs.size && i >= 0) {
      val tree = CherryTree(xs: _*)
      tree(i) shouldBe xs(i)
    }
  }

  it should "concat elements" in forAll { (xs: List[Int], ys: List[Int]) =>
    CherryTree(xs: _*) concat CherryTree(ys: _*) shouldBe CherryTree(xs ++ ys: _*)
  }

  it should "get correct size" in forAll { (xs: Vector[Int]) =>
    CherryTree(xs: _*).size shouldBe xs.size
  }

  it should "sum elements" in forAll { (xs: Vector[Int]) =>
    CherryTree(xs: _*).sum shouldBe xs.sum
  }

  it should "fold left (multiply) elements" in forAll { (xs: Vector[Int]) =>
    def foo = {(x: Int, r: Int) => r * x}
    CherryTree(xs: _*).foldLeft(1)(foo) shouldBe xs.foldLeft(1)(foo)
  }
}
