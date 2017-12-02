package lectures.concurrent.akka.training

import scala.concurrent.{Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import scala.concurrent.duration._
import scala.util.Random
object Example extends App {

  def sleep(time: Long) { Thread.sleep(time) }
  println("starting calculation ...")
  val f = Future {
    sleep(Random.nextInt(500))
    42
  }
  println("before onComplete")
  f.onComplete {
    case Success(value) => println(s"Got the callback, meaning = $value")
    case Failure(e) => e.printStackTrace
  }
  // do the rest of your work
  println("A ..."); sleep(100)
  println("B ..."); sleep(100)
  println("C ..."); sleep(100)
  println("D ..."); sleep(100)
  println("E ..."); sleep(100)
  println("F ..."); sleep(100)
  sleep(2000)
}

