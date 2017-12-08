package lectures.akka.chat

import akka.event.Logging
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.http.scaladsl.server.Directives._
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{ActorMaterializer, Attributes, OverflowStrategy}
import akka.typed.ActorRef
import akka.typed.scaladsl.AskPattern._
import akka.typed.scaladsl.adapter._
import akka.util.Timeout
import akka.{NotUsed, actor => untyped}
import com.typesafe.config.ConfigFactory
import io.circe.parser._
import io.circe.syntax._
import lectures.akka.chat.Chat.Disconnect

import scala.concurrent.duration._
import scala.io.StdIn

object ChatServer {
  implicit val system = untyped.ActorSystem("websocket-chat", ConfigFactory.load("chat.conf"))
  implicit val materializer = ActorMaterializer()
  implicit val timeout = Timeout(1.second)
  implicit def sched = system.scheduler

  import system.dispatcher

  val hub = Hub()

  def chatUser(hub: ActorRef[Hub.Message]): Flow[Message, Message, NotUsed] = {
    val sessionRef = ActorRef(hub ? Hub.NewSession)

    val input: Sink[Message, NotUsed] = Flow[Message]
      .collect { case mes: TextMessage => mes }
      .mapAsync(1) { mes =>
        val builder = StringBuilder.newBuilder
        mes.textStream.runForeach(builder ++= _).map(_ => builder.result())
      }
      .map(parse)
      .collect { case Right(json) => json.as[In] }
      .collect { case Right(in) => Session.Input(in) }
      .log("decode")
      .withAttributes(Attributes.logLevels(onElement = Logging.InfoLevel))
      .toMat(Sink.foreach { in => sessionRef ! in }) {
        case (_, fd) =>
          fd.onComplete { _ =>
            println("disconnect")
            sessionRef ! Session.Disconnect
          }
          NotUsed
      }

    val output: Source[Message, NotUsed] =
      Source.actorRef[Out](1000, OverflowStrategy.dropHead)
        .mapMaterializedValue { ref =>
          sessionRef ! Session.Init(hub, ref)
          NotUsed
        }.map(mess => TextMessage(mess.asJson.spaces2))

    Flow.fromSinkAndSource(input, output)
  }

  val route = path("chat") {
    handleWebSocketMessages(chatUser(hub))
  }

  def main(args: Array[String]): Unit = {
    Http().bindAndHandle(route, "0.0.0.0", 9889).flatMap { bnd =>
      println(s"bound to ${bnd.localAddress}")
      println(s"enter `stop` to shutdown")
      while (StdIn.readLine().trim != "stop") {}
      bnd.unbind()
    }
      .flatMap(_ => system.terminate())
      .flatMap(_ => hub.terminate())
  }
}





