package inclasss

import lectures.akka.chat._
import io.circe.syntax._

object Circe extends App {
  println(Map[String, In](
    "a" -> In.Create("room1"),
    "b" -> In.Send("hello"),
    "c" -> In.Enter("vasia", "room1")
  ).asJson)

}
