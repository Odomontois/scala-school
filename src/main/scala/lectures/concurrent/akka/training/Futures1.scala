package lectures.concurrent.akka.training

// 1 - the imports
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
object Futures1 extends App {
  // used by 'time' method
  implicit val baseTime = System.currentTimeMillis
  // 2 - create a Future
  val f = Future {
    Thread.sleep(500)
    1 + 1
  }
  println(f.value)
  // 3 - this is blocking (blocking is bad)
  val result = Await.result(f, 1 second)
  println(result)
  println(f.value)
  Thread.sleep(1000)
}
