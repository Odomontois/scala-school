package inclasss

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.function.UnaryOperator

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._

object Futures {

  val sum = new AtomicInteger(0)
  val count = new AtomicInteger(0)
  val list = new AtomicReference[List[Int]](Nil)


  def add(x: Int) = {
    count.addAndGet(1)
    sum.addAndGet(x)
    list.updateAndGet(new UnaryOperator[List[Int]] {
      override def apply(t: List[Int]): List[Int] = x :: t
    })
  }

  val futList = 1 to 100 map { i =>
    Future(add(i))
  }

  def main(args: Array[String]): Unit = {
    val start = System.nanoTime()
    val x: Future[String] = {
      Future(1).flatMap(_ => throw new Exception("no string today"))
    }
    val y = x.recoverWith{
      case ex: Exception => Future(ex.getMessage)
    }
    Await.ready(x, Duration.Inf)
    println(x)
    println(y)
    println(s"elapsed ${(System.nanoTime() - start) / 1e6} millis")
  }
}
