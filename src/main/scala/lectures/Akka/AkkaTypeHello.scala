//package lectures.Akka
//
////import akka.actor.{Actor, ActorSystem}
//import akka.typed.{ActorSystem,Behavior}
//import akka.typed.scaladsl.{Actor}
//
//
//object AkkaTypeHello {
//
//
//
//  def main(args: Array[String]): Unit  ={
//
//
//    val system = ActorSystem[String](
//      Actor.immutable{(ctx,word) =>
//        val actor = ctx.spawn(greeter(word),word)
//        Actor.same
//      },
//      "hello"
//    )
//
//  }
//}
