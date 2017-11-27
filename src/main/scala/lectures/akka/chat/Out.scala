package lectures.akka.chat

import io.circe.{Decoder, Encoder}
import io.circe.generic.JsonCodec
import io.circe.generic.extras.semiauto.deriveEncoder

sealed trait Out

object Out {
  @JsonCodec
  final case class ChatRoomList(names: List[String]) extends Out

  @JsonCodec
  final case class NewPersonEntered(name: String) extends Out

  @JsonCodec
  final case class MessageSent(login: String, message: String) extends Out

  @JsonCodec
  final case class Connected(chat: String) extends Out

  implicit val circeConfig = io.circe.generic.extras.Configuration.default
                             .withDiscriminator("type")
                             .withSnakeCaseConstructorNames

  implicit val encoder: Encoder[Out] = deriveEncoder
}
