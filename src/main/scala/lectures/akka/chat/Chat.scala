package lectures.akka.chat

import akka.typed.scaladsl.Actor
import akka.typed.scaladsl.Actor.MutableBehavior
import akka.typed.{ActorRef, Behavior, scaladsl}

object Chat {
  sealed trait Message

  final case class SendText(login: String, text: String) extends Message
  final case class Connect(login: String, session: ActorRef[Session.Message]) extends Message
  final case class Disconnect(session: ActorRef[Session.Message]) extends Message

  def behavior(name: String): Behavior[Message] =
    Actor.mutable(ctx => new Behave(ctx, name))

  private class Behave(ctx: scaladsl.ActorContext[Message], name: String) extends MutableBehavior[Message] {
    var sessions = Map.empty[ActorRef[Session.Message], String]

    def onMessage(msg: Message): Behavior[Message] = {
      msg match {
        case SendText(login, text)   =>
          sessions.foreach{ case (k, _) => k ! Session.NewMessage(login, text)}
        case Connect(login, session) =>
          (sessions - session).foreach{case (k, _) => k ! Session.UserEnter(login)}
          sessions += session -> login
          ctx.watchWith(session, Disconnect(session))
        case Disconnect(session)     =>
          val login = sessions(session)
          sessions -= session
          sessions.foreach{case (sess, _) => sess ! Session.UserLeave(login)}
          ctx.unwatch(session)
      }
      Actor.same
    }
  }
}
