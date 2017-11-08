package lectures.collections

import org.scalatest.{BeforeAndAfter, GivenWhenThen, Matchers, WordSpec}

/**
  *
  *
  *
  */
class MergeSortImplWordSpecTest extends WordSpec with Matchers with GivenWhenThen with BeforeAndAfter{ //BeforeAndAfterAll

  before {
    // here we can put initialization of some static resources
    // or resources that are accessible by tests
  }

  after{
    // some finalization
  }
  def seqTest(f: (Seq[Int]) => Unit) = f(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9))

  "A seq" when {
    "shuffled" should {
      "not be equal to sorted seq" in seqTest { defaultSeq =>
        val shuffled = scala.util.Random.shuffle(defaultSeq)
        shuffled should not equal defaultSeq
      }
    }
    "sorted with an Impl" should {

      "be equal to already sorted seq" in pendingUntilFixed {
        seqTest { defaultSeq => {
          val shuffled = scala.util.Random.shuffle(defaultSeq)
          shuffled should not equal defaultSeq

          val sorted = MergeSortImpl.mergeSort(shuffled)
          sorted shouldBe defaultSeq
        }
        }
      }

      "have the same size as original seq" in pendingUntilFixed {
        seqTest { defaultSeq => {
          val shuffled = scala.util.Random.shuffle(defaultSeq)
          shuffled should not equal defaultSeq

          val sorted = MergeSortImpl.mergeSort(shuffled)
          sorted should not equal shuffled
          sorted should have size shuffled.size
        }
        }
      }

      "throw exception" in {
        seqTest { defaultSeq => {
          When("index requested exceed it's size")
          val shuffled = scala.util.Random.shuffle(defaultSeq)
          shuffled should not equal defaultSeq

          intercept[IndexOutOfBoundsException] {
            val sorted = MergeSortImpl.mergeSort(shuffled)
            sorted should not equal shuffled
            sorted(shuffled.length)
          }
        }
        }
      }
    }
  }
}

