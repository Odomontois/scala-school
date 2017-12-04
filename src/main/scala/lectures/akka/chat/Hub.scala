package lectures.akka.chat

import akka.typed.scaladsl.Actor
import akka.typed.scaladsl.Actor.MutableBehavior
import akka.typed.{ActorRef, ActorSystem, Behavior, Signal, Terminated, scaladsl}
import akka.{actor => untyped}
import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging

object Hub extends LazyLogging {
  sealed trait Message

  final case class GetChats(session: ActorRef[Session.Message]) extends Message
  final case class Connect(name: String, login: String, session: ActorRef[Session.Message]) extends Message
  final case class NewSession(respond: ActorRef[ActorRef[Session.Message]]) extends Message
  final case class NewChat(name: String) extends Message

  def adapt()(implicit usys: untyped.ActorSystem): ActorSystem[Hub.Message] =
    ActorSystem.adapter[Message]("chat-hub", behaiour)

  def apply(name: String = "chat-hub", config: Option[Config] = None) = ActorSystem(behaiour, name, config = config)

  def behaiour: Behavior[Message] = Actor.mutable(ctx => new Behave(ctx))


  class Behave(ctx: scaladsl.ActorContext[Message]) extends MutableBehavior[Message] {
    var chats = Map.empty[String, ActorRef[Chat.Message]]
    def onMessage(msg: Message): Behavior[Message] = {
      msg match {
        case GetChats(session)             => session ! Session.ChatList(chats.keys.toList)
        case Connect(name, login, session) =>
          chats.get(name).foreach { chat =>
            chat ! Chat.Connect(login, session)
            session ! Session.Connected(name, login, chat)
          }
        case NewSession(respond)           =>
          logger.info("new client connected")
          val session = ctx.spawnAnonymous(Session.initial)
          ctx.watch(session)
          respond ! session
        case NewChat(name)                 =>
          chats += name -> ctx.spawn(Chat.behavior(ctx, name), s"chat-$name")
      }
      Actor.same
    }

    def onSignal(signal: Signal): Behavior[Message] = {
      signal match {
        case Terminated(session) =>
      }
      Actor.same
    }
  }

}
