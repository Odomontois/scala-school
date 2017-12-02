package lectures.concurrent.akka.training

import akka.actor._
class Kenny extends Actor {
  println("entered the Kenny constructor")
  override def preStart { println("kenny: preStart") }
  override def postStop { println("kenny: postStop") }
  override def preRestart(reason: Throwable, message: Option[Any]) {
    println("kenny: preRestart")
    println(s" MESSAGE: ${message.getOrElse("")}")
    println(s" REASON: ${reason.getMessage}")
    super.preRestart(reason, message)
  }
  override def postRestart(reason: Throwable) {
    println("kenny: postRestart")
    println(s" REASON: ${reason.getMessage}")
    super.postRestart(reason)
  }
  def receive = {
    case ForceRestart => throw new Exception("Boom!")
    case _ => println("Kenny received a message")
  }
}
case object ForceRestart
object LifecycleDemo extends App {
  val system = ActorSystem("LifecycleDemo")
  val kenny = system.actorOf(Props[Kenny], name = "Kenny")
  println("sending kenny a simple String message")
  kenny ! "hello"
  Thread.sleep(1000)
  println("make kenny restart")
  kenny ! ForceRestart
  Thread.sleep(1000)
  println("stopping kenny")
  system.stop(kenny)
  println("shutting down")
}
