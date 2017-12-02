package lectures.concurrent.akka.training.typedAkka
import ChatRoom._
import akka.typed.{ActorSystem, Behavior, Terminated}
import akka.typed.scaladsl.Actor
import scala.concurrent.duration._

import scala.concurrent.Await

object ChatRoomTest1 extends App{

  val gabbler =
    Actor.immutable[SessionEvent] { (_, msg) ⇒
      msg match {
        case SessionGranted(handle) ⇒
          handle ! PostMessage("Hello World!")
          Actor.same
        case MessagePosted(screenName, message) ⇒
          println(s"message has been posted by '$screenName': $message")
          Actor.stopped
      }
    }

  val main: Behavior[akka.NotUsed] =
    Actor.deferred { ctx ⇒
      val chatRoom = ctx.spawn(ChatRoom.behavior, "chatroom")
      val gabblerRef = ctx.spawn(gabbler, "gabbler")
      ctx.watch(gabblerRef)
      chatRoom ! GetSession("ol’ Gabbler", gabblerRef)

      Actor.immutable[akka.NotUsed] {
        (_, _) ⇒ Actor.unhandled
      } onSignal {
        case (ctx, Terminated(ref)) ⇒
          Actor.stopped
      }
    }

  val system = ActorSystem(main, "ChatRoomDemo")
  Await.result(system.whenTerminated, 3.seconds)

}
