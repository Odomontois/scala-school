package lectures.oop

import org.scalatest.{Matchers, WordSpec}

/**
  * Раскомментируйте и допишите тесты на
  * класс lectures.oop.Application
  */
class ApplicationTest extends WordSpec with Matchers {

  private val started = new AfterWord("started")

  "Application" should {
    "return correct answer" when started{
      "in a test environment" in {
        new Application(true).doTheJob() shouldBe 5
      }
      "in a production environment" in {
        new Application(false).doTheJob() shouldBe 2
      }
    }
  }
}
