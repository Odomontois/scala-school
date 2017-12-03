package lectures.akka.chat

import akka.actor.Scheduler
import akka.typed.scaladsl.AskPattern._
import akka.typed.scaladsl.{Actor, ActorContext}
import akka.typed.{ActorRef, Behavior}
import akka.util.Timeout
import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.ExecutionContext

object Session extends LazyLogging{
  import scala.concurrent.duration._
  sealed trait Message

  final case class ChatList(names: List[String]) extends Message
  final case class Connected(name: String, login: String, chat: ActorRef[Chat.Message]) extends Message
  final case class UserEnter(name: String) extends Message
  final case class UserLeave(name: String) extends Message
  final case class NewMessage(login: String, text: String) extends Message
  final case class Input(in: In) extends Message
  final case class Init(hub: ActorRef[Hub.Message], out: ActorRef[Out]) extends Message
  case object Disconnect extends Message

  val initial: Behavior[Message] = Actor.immutable {
    case (_, Init(hub, out)) =>
      logger.debug("initialized")
      initialized(hub, out)
    case _                   => Actor.same
  }


  private def initialized(hub: ActorRef[Hub.Message], out: ActorRef[Out]): Behavior[Message] = {

    def common(ctx: ActorContext[Message]): Message => Behavior[Message] = {
      case ChatList(names)                 =>
        out ! Out.ChatRoomList(names)
        Actor.same
      case Connected(name, login, newChat) =>
        out ! Out.Connected(name)
        inChat(newChat, login)
      case Input(in)                       =>
        import In._
        in match {
          case Enter(newLogin, name) =>
            hub ! Hub.Connect(name, newLogin, ctx.self)
          case Create(name)          =>
            implicit val ec: ExecutionContext = ctx.executionContext
            implicit val timeout: Timeout = Timeout(1.second)
            implicit val sch: Scheduler = ctx.system.scheduler
            val future: scala.concurrent.Future[Boolean] = hub ? (Hub.NewChat(name, _))
            future.map(if (_) "success" else "failure").foreach(out ! Out.CreateResponse(_))
          case Channels()            => hub ! Hub.GetChats(ctx.self)

        }
        Actor.same
      case _                               => Actor.same
    }

    def inChat(chat: ActorRef[Chat.Message], login: String): Behavior[Message] = Actor.immutable { (ctx, mess) =>
      mess match {
        case NewMessage(author, text) =>
          out ! Out.MessageSent(author, text)
          Actor.same
        case UserEnter(userLogin)     =>
          out ! Out.NewPersonEntered(userLogin)
          Actor.same
        case Input(In.Send(text))     =>
          chat ! Chat.SendText(login, text)
          Actor.same
        case msg                      => common(ctx)(msg)
      }
    }

    val inLobby: Behavior[Message] = Actor.immutable { (ctx, mess) => common(ctx)(mess) }

    inLobby
  }
}
