package lectures.matching

import lectures.matching.SortingStuff._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

import scala.util.Random


/**
  * Короткий список самых востребованных генераторов:
  * Gen.alphaString
  * Gen.delay
  * Gen.oneOf
  * Gen.resultOf
  * Gen.zip
  * Gen.map
  * Gen.suchThat
  * Gen.mapOf
  * Gen.pic
  * Gen.choose
  *
  * Допишите 2 теста:
  * "find knife" . Тест должен показать, что если нож есть в вещах,
  * то метод findMyKnife его отыщет.
  *
  * Для "put boots ..." создайте генератор и проверьте правильность работы метода sortJunk по аналогии с предыдущими тестами.
  *
  */

class SortingStuffGeneratorBasedTest extends WordSpec with Matchers with PropertyChecks {

  val cheepWatchGen: Gen[Watches] = Gen.zip(Gen.choose(0f, 1000f), Gen.alphaStr).map(w => Watches(w._2, w._1))
  val bookGenerator = Gen.alphaStr.map(name => Book(name, Random.nextBoolean()))
  val interestingBookGen = bookGenerator.filter(_.isInteresting)
  val coolbootsGen = Gen.oneOf("Adidas","Converse").map(name => Boots(name,35 + Random.nextInt(11)))
  val otherbootsGen = Gen.oneOf("Abijas","Conwirse","Nike","NoName121").map(name => Boots(name,35 + Random.nextInt(11)))


  // Override configuration if you need
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(minSize = 10, maxSize = 20)

  val get: AfterWord = new AfterWord("have")

  "This test" should get {
    "proper cheep watch generator" in {
      forAll(cheepWatchGen) { (watch: Watches) => {
        watch.cost should be <= 1000f
      }
      }
    }
    "proper interesting book generator" in {
      val books = interestingBookGen
      forAll(books) { (book: Book) => {
        book shouldBe 'interesting
      }
      }
    }
  }

  "Sort stuff" should {
    "return collections" which {
      "total size is equal to item amount" in /*pendingUntilFixed*/{
        val ms = generatorDrivenConfig.minSuccessful

        val books = (1 to ms) flatMap { _ => interestingBookGen.sample }
        val watches = (1 to ms) flatMap { _ => cheepWatchGen.sample }

        val StuffBox(goodBooks, niceWatches, _, junk) = SortingStuff.sortJunk(Random.shuffle(books ++ watches).toList)
        goodBooks should have size books.size
        niceWatches should have size 0
        junk should have size watches.size
      }
    }

    "findMyKnife" should {
      "find knife" when {
        "it was put to the box" in {
          val ms = generatorDrivenConfig.minSuccessful
          val books = (1 to ms) flatMap { _ => bookGenerator.sample }
          val watches = (1 to ms) flatMap { _ => cheepWatchGen.sample }
          SortingStuff.findMyKnife(SortingStuff.sortJunk(Random.shuffle(books ++ watches).toList)) should equal(false)
          SortingStuff.findMyKnife(SortingStuff.sortJunk(Random.shuffle(books ++ watches ++ Traversable(Knife)).toList)) should equal(true)
        }
      }
    }

    "put boots in a proper place" when {
      "boots were produced by Converse or Adidas" in {
        val ms = generatorDrivenConfig.minSuccessful
        val coolBoots = (1 to ms) flatMap { _ => coolbootsGen.sample }
        val otherBoots = (1 to ms) flatMap { _ => otherbootsGen.sample }


        val StuffBox(goodBooks, niceWatches, sortedCoolboots, junk) =
          SortingStuff.sortJunk(Random.shuffle(coolBoots ++ otherBoots).toList)
        sortedCoolboots should have size coolBoots.size
        junk should have size otherBoots.size
      }
    }
  }
}
