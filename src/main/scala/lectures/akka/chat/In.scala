package lectures.akka.chat

import io.circe.{Decoder, Encoder}
import io.circe.generic.JsonCodec
import io.circe.generic.extras.semiauto._


sealed trait In

object In {
  @JsonCodec
  final case class Channels() extends In

  @JsonCodec
  final case class Enter(login: String, chat: String) extends In

  @JsonCodec
  final case class Create(chat: String) extends In

  @JsonCodec
  final case class Send(text: String) extends In

  implicit val circeConfig = io.circe.generic.extras.Configuration.default
    .withDiscriminator("type")
    .withSnakeCaseConstructorNames

  implicit val decoder: Decoder[In] = deriveDecoder
  implicit val encoder: Encoder[In] = deriveEncoder
}
