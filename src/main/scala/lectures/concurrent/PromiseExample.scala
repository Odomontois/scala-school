package lectures.concurrent

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}
import scala.util.Success

object PromiseExample extends App {

  val p = Promise[Int]()
  val f = p.future

  var doJob = true
  // Imitation of endless async process.
  val runnable = new Runnable {
    override def run(): Unit = {
      var i = 0
      while (doJob) {
        i = i + 1
        Thread.sleep(100)
        if (i > 100) p.tryComplete(Success(i)) //p.complete(Success(i))
      }
    }
  }

  val tr = new Thread(runnable)
  tr.start()
  val r = Await.ready(f, Duration.Inf)
  println("RESULT IS" + r)
  doJob = false
}
