package io.hnfmr.chapter5

import cats.data.EitherT

import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._

import cats.instances.future._
import scala.concurrent.ExecutionContext.Implicits.global

object MTExercise extends App {

  type Response[A] = EitherT[Future, String, A]

  val powerLevels: Map[String, Int] = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] = {
    powerLevels.get(autobot) match {
      case Some(lvl) => EitherT.right(Future(lvl)) // ~> Future(Right(lvl))
      case None      => EitherT.left(Future(s"$autobot not reachable")) // ~> Future(Left(...))

    }
  }

  val f = Future.sequence(
    List(
      getPowerLevel("xxx").value,
      getPowerLevel("Jazz").value
    )
  )

  val rr = Await.result(f, 2.seconds)
  println(rr)


  def canSpecialMove( ally1: String, ally2: String ): Response[Boolean] = {
    for {
      a <- getPowerLevel(ally1)
      b <- getPowerLevel(ally2)
    } yield (a+b) > 15
  }

  println(Await.result(canSpecialMove("Jazz", "Bumblebee").value, 1.second))
  println(Await.result(canSpecialMove("Jazz", "xxx").value, 1.second))
  println(Await.result(canSpecialMove("Jazz", "Hot Rod").value, 1.second))

  def tacticalReport(ally1: String, ally2: String): String = {
    val r = Await.result(canSpecialMove(ally1, ally2).value, 1.second)
    r match {
      case Right(ready) => if (ready) s"$ally1 and $ally2 are ready to roll out" else s"$ally1 and $ally2 need a recharge"
      case Left(err) => s"Comms error: $err"
    }
  }

  println(tacticalReport("Jazz", "Bumblebee"))
  println(tacticalReport("Hot Rod", "Bumblebee"))
  println(tacticalReport("xxx", "Bumblebee"))
}


