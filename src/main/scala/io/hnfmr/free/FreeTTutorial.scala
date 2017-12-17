package io.hnfmr.free

import cats.free._
import cats._
import cats.data._

import scala.language.higherKinds

object FreeTTutorial extends App {

  // Data
  sealed abstract class Teletype[A] extends Product with Serializable

  final case class WriteLine(line: String) extends Teletype[Unit]

  final case class ReadLine(prompt: String) extends Teletype[String]

  type TeletypeT[M[_], A] = FreeT[Teletype, M, A]
  type Log = List[String]

  type TeletypeState[A] = State[Log, A]

  // Smart constructors
  object TeletypeOps {
    def writeLine(line: String): TeletypeT[TeletypeState, Unit] =
      FreeT.liftF[Teletype, TeletypeState, Unit](WriteLine(line))

    def readLine(prompt: String): TeletypeT[TeletypeState, String] =
      FreeT.liftF[Teletype, TeletypeState, String](ReadLine(prompt))

    def log(s: String): TeletypeT[TeletypeState, Unit] =
      FreeT.liftT[Teletype, TeletypeState, Unit](State.modify(s :: _))

    def program: TeletypeT[TeletypeState, Unit] = {
      for {
        userSaid <- TeletypeOps.readLine("what's up?!")
        _ <- log(s"user said: $userSaid")
        _ <- TeletypeOps.writeLine("thanks, see you soon!")
        _ <- log("Finished!")
      } yield ()
    }

    def interpreter = new (Teletype ~> TeletypeState) {
      def apply[A](fa: Teletype[A]): TeletypeState[A] = {
        fa match {
          case ReadLine(prompt) =>
            println(prompt)
            val userInput: String = scala.io.StdIn.readLine()
            StateT.pure[Eval, List[String], A](userInput)
          case WriteLine(line) =>
            StateT.pure[Eval, List[String], A](println(line))
        }
      }
    }
  }

  import TeletypeOps._

  val state = program.foldMap(interpreter)
  val initialState = Nil
  val (stored, _) = state.run(initialState).value
  println(stored)
}
