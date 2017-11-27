package inclasss

import akka.actor.{ActorRef, ActorSystem, PoisonPill}
import akka.stream.{ActorMaterializer, ClosedShape, OverflowStrategy}
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, Keep, Merge, Sink, Source}
import com.typesafe.config.{Config, ConfigFactory}
import GraphDSL.Implicits._
import akka.stream.javadsl.RunnableGraph

import scala.io.StdIn

object Streams {
  implicit val system = ActorSystem("streams", ConfigFactory.load("chat.conf"))
  implicit val materializer = ActorMaterializer()

  import system.dispatcher
  import Numeric.Implicits._

  val simpleSource = Source(List(1, 2, 3))
  val actorSource = Source.actorRef[String](100, OverflowStrategy.dropNew)

  val parseInt = Flow[String]
    .map(s => util.Try(s.toInt).toOption)
    .collect { case Some(x) => 2 * x + 1 }

  val parseDouble = Flow[String]
    .map(s => util.Try(s.toDouble))
    .collect { case util.Success(d) => d }

  val sinkPrint = Sink.foreach(println)

  def sinkSum[N](implicit N: Numeric[N]) = Sink.fold[N, N](N.zero)(_ + _)

  val graph =
    actorSource
      .alsoTo(sinkPrint)
      .via(parseInt)
      .alsoTo(sinkPrint)
      .toMat(sinkSum)(Keep.both)

  val graph2 =
    GraphDSL
      .create(actorSource, sinkSum[Int], sinkSum[Double]) { case triple => triple } {
        implicit b =>
          (src, sumInt, sumDouble) =>
            val bcast = b.add(Broadcast[String](2))
            val bcastInt = b.add(Broadcast[Int](2))
            val bcastDouble = b.add(Broadcast[Double](2))
            val merge = b.add(Merge[String](2))

            src ~> bcast

            bcast ~> parseInt ~> bcastInt

            bcastInt ~> sumInt

            bcast ~> parseDouble ~> bcastDouble

            bcastDouble ~> sumDouble

            bcastInt ~> Flow[Int].map(i => s"int $i") ~> merge

            bcastDouble ~> Flow[Double].map(d => s"double $d") ~> merge

            merge ~> Sink.foreach(println)

            ClosedShape
      }

  def readInputTo(ref: ActorRef): Unit = {
    val l = StdIn.readLine().trim
    if (l == "stop") {
      ref ! PoisonPill
    } else {
      ref ! l
      readInputTo(ref)
    }
  }

  def main(args: Array[String]): Unit = {
    val (ref, intSum, doubleSum) = RunnableGraph.fromGraph(graph2).run(materializer)
    readInputTo(ref)
    intSum.onComplete { x =>
      println(s"int sum is $x")
    }
    doubleSum.onComplete { x =>
      println(s" double sum is $x")
    }

    for (_ <- intSum; _ <- doubleSum) system.terminate()

  }

}
