package lectures.akka.chat

import akka.typed.scaladsl.Actor
import akka.typed.scaladsl.Actor.MutableBehavior
import akka.typed.{ActorRef, Behavior}

object Chat {
  sealed trait Message

  final case class Users(rec:  ActorRef[Session.UserList]) extends Message
  final case class SendText(login: String, text: String) extends Message
  final case class Connect(login: String, session: ActorRef[Session.Message]) extends Message
  final case class Disconnect(session: ActorRef[Session.Message]) extends Message

  def behavior(name: String): Behavior[Message] = Actor.mutable(_ => new Behave(name))

  private class Behave(name: String) extends MutableBehavior[Message] {
    var sessions = Map.empty[ActorRef[Session.Message], String]

    def onMessage(msg: Message): Behavior[Message] = {
      msg match {
        case SendText(login, text)   =>
          sessions.foreach{ case (k, _) => k ! Session.NewMessage(login, text)}
        case Connect(login, session) =>
          (sessions - session).foreach(_._1 ! Session.UserEnter(login))
           sessions += (session->login)
        case Disconnect(session)     =>
          sessions -= session
        case Users(rec) =>
          rec ! Session.UserList(sessions.values.toList, name)
          sessions.values.toList.foreach(println);
      }
      Actor.same
    }

  }
}