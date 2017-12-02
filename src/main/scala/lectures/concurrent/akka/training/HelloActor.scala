package lectures.concurrent.akka.training

import akka.actor.{Actor, ActorSystem, Props}
class HelloActor extends Actor {
  def receive = {
    case "hello" => println("hello back at you")
    case _       => println("huh?"); println(sender)
  }
}
object Main extends App {
  // an actor needs an ActorSystem
  val system = ActorSystem("HelloSystem")
  // create and start the actor
  val helloActor = system.actorOf(Props[HelloActor], name = "helloactor")
  // send the actor two messages
  helloActor ! "hello"
  helloActor ! "buenos dias"
  system.terminate()
  // shut down the system
  //system.shutdown
}

