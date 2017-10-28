package lectures.collections

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

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
    else xs.tail shouldBe CherryTree(xs.tail: _*)
  }

  it should "get head" in forAll { xs: Vector[Int] =>
    val tree = CherryTree(xs: _*)
    if (xs.isEmpty) an[NoSuchElementException] should be thrownBy tree.head
    else xs.head shouldBe tree.head
  }

  it should "get init" in forAll { xs: Vector[Int] =>
    val tree = CherryTree(xs: _*)
    if (xs.isEmpty) an[UnsupportedOperationException] should be thrownBy tree.init
    else xs.init shouldBe CherryTree(xs.init: _*)
  }

  it should "get last" in forAll { xs: Vector[Int] =>
    val tree = CherryTree(xs: _*)
    if (xs.isEmpty) an[NoSuchElementException] should be thrownBy tree.last
    else xs.last shouldBe tree.last
  }

  it should "get element by index" in forAll { (xs: Vector[Int], i: Int) =>
    val tree = CherryTree(xs: _*)
    if (i < 0 || i >= tree.size) an[IndexOutOfBoundsException] should be thrownBy tree(i)
    else tree(i) shouldBe xs(i)
  }

  it should "concat elements" in forAll { (xs: List[Int], ys: List[Int]) =>
    CherryTree(xs: _*) concat CherryTree(ys: _*) shouldBe CherryTree(xs ++ ys: _*)
  }

  it should "get correct size" in forAll { (xs: Vector[Int]) =>
    CherryTree(xs: _*).size shouldBe xs.size
  }

  it should "get drop" in forAll { (xs: Vector[Int], i: Int) =>
    val tree = CherryTree(xs: _*)
    tree.drop(i) shouldBe CherryTree(xs.drop(i): _*)
  }

  it should "get take" in forAll { (xs: Vector[Int], i: Int) =>
    val tree = CherryTree(xs: _*)
    tree.take(i) shouldBe CherryTree(xs.take(i): _*)
  }

}
