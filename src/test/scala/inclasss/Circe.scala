package inclasss

import lectures.akka.chat._
import io.circe.syntax._

object Circe extends App {
  println(Map[String, In](
    "a" -> In.Create("foooasdasd  asd"),
    "b" -> In.Send("1231 sdfds !!!"),
    "c" -> In.Enter("lolo", "uuu")
  ).asJson)

}
