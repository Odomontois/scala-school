package lectures.akka.chat

import akka.typed.scaladsl.Actor
import akka.typed.scaladsl.Actor.MutableBehavior
import akka.typed.{ActorRef, Behavior}

object Chat {
  sealed trait Message

  final case class SendText(login: String, text: String) extends Message
  final case class Connect(login: String, session: ActorRef[Session.Message]) extends Message
  final case class Disconnect(session: ActorRef[Session.Message]) extends Message
  final case class SendWhisper(form: String, to: String, text: String) extends Message

  def behavior(name: String): Behavior[Message] = Actor.mutable(_ => new Behave(name))

  private class Behave(name: String) extends MutableBehavior[Message] {
    var sessions = Map.empty[ActorRef[Session.Message], String]

    def onMessage(msg: Message): Behavior[Message] = {
      msg match {
        case SendText(login, text)   =>
          sessions.foreach{ case (k, _) => k ! Session.NewMessage(login, text)}
        case Connect(login, session) =>
          (sessions - session).foreach{case (k, _) => k ! Session.UserEnter(login)}
          sessions += session -> login
        case Disconnect(session)     =>
          sessions -= session
        case SendWhisper(from, to, text) =>
          val tos = sessions.collect{case (k, v) if v == to => k}
          tos.foreach{ session =>
            session ! Session.NewWhisper(from, text)
          }
      }
      Actor.same
    }

  }
}
