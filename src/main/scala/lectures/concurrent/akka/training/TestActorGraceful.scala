
package lectures.concurrent.akka.training

import akka.actor._
import akka.pattern.gracefulStop
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.language.postfixOps
class TestActorGraceful extends Actor {
  def receive = {
    case _ => println("TestActor got message")
  }
  override def postStop { println("TestActor: postStop") }
}


object GracefulStopTest extends App {
  val system = ActorSystem("GracefulStopTest")
  val testActor = system.actorOf(Props[TestActorGraceful], name = "TestActor")
  // try to stop the actor gracefully
  try {
    val stopped: Future[Boolean] = gracefulStop(testActor, 5 seconds)//(system)
    Await.result(stopped, 1 seconds)
    println("testActor was stopped")
  } catch {
    case e:Exception => e.printStackTrace
  } finally {
    println("finally")
    //system.shutdown
    system.terminate();


  }
}
