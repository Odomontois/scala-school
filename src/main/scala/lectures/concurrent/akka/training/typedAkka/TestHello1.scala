package lectures.concurrent.akka.training.typedAkka




import HelloWorld._
//import akka.typed.ActorSystem
import akka.typed._

import akka.typed.scaladsl.AskPattern._
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.Future
// using global pool since we want to run tasks after system.terminate
import scala.concurrent.ExecutionContext.Implicits.global
object TestHello1 extends App{
  implicit val timeout: Timeout = Timeout(5 seconds)

  val system: ActorSystem[Greet] = ActorSystem(greeter, "hello")

  implicit val scheduler = system.scheduler

  val future: Future[Greeted] = system ? (Greet("world", _))
  for {
    greeting ← future.recover { case ex ⇒ ex.getMessage }
    done ← {
      println(s"result: $greeting"); system.terminate()
    }
  } println("system terminated")
}