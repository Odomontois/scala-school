package lectures.concurrent.akka.training.typedAkka

import akka.typed._
import akka.typed.scaladsl.Actor

object HelloWorld {
  final case class Greet(whom: String, replyTo: ActorRef[Greeted])
  final case class Greeted(whom: String)

  val greeter = Actor.immutable[Greet] { (_, msg) ⇒
    println(s"Hello ${msg.whom}!")
    msg.replyTo ! Greeted(msg.whom)
    Actor.same
  }
}
