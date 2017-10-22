package lectures.check

import org.scalatest.{OutcomeOf, Matchers, WordSpec}
import org.scalatest.prop.PropertyChecks

class Fraction(n: Int, d: Int) {

  require(d != 0)
  require(d != Integer.MIN_VALUE)
  require(n != Integer.MIN_VALUE)

  val numer = if (d < 0) -1 * n else n
  val denom = d.abs

  override def toString = numer + " / " + denom
}

class TableStyleScalaCheckTest extends WordSpec with PropertyChecks with Matchers with OutcomeOf {

  "Fraction" should {
    "work correctly" when {
      "numerator and denominator are correct" in {

        forAll { (n: Int, d: Int) =>
          println(s"n == $n and  d == $d ")
          whenever(d != 0 && d != Integer.MIN_VALUE
            && n != Integer.MIN_VALUE) {

            val f = new Fraction(n, d)
            println(s"do a fraction $f")

            if (n < 0 && d < 0 || n > 0 && d > 0)
              f.numer should be > 0
            else if (n != 0)
              f.numer should be < 0
            else
              f.numer should equal(0)

            f.denom should be > 0
          }
        }
      }
    }
    "produce IllegalArgumentException" when {
      "illegal input has been presented" in {
        val invalidCombos =
          Table(
            ("n", "d"),
            (Integer.MIN_VALUE, Integer.MIN_VALUE),
            (1, Integer.MIN_VALUE),
            (Integer.MIN_VALUE, 1),
            (Integer.MIN_VALUE, 0),
            (1, 0)
          )

//        val o = outcomeOf{"blaa" shouldNot be ("blaa")}

        forAll(invalidCombos) { (n: Int, d: Int) =>
          an[IllegalArgumentException] should be thrownBy {
            new Fraction(n, d)
          }
        }
      }
    }
  }
}

