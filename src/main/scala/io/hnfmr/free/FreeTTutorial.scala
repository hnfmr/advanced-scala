package io.hnfmr.free

import cats.free._
import cats._
import cats.data._

object FreeTTutorial extends App {
  sealed abstract class Teletype[A] extends Product with Serializable
}
