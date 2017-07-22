package io.hnfmr.chapter4

import cats.data.Reader
import cats.syntax.applicative._

object ReaderTut extends App {
  case class Db( usernames: Map[Int, String], passwords: Map[String, String] )

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader( db => {
      db.usernames.get(userId)
    })

  def checkPasswordOpt(username: Option[String], password: String): DbReader[Boolean] =
    Reader(db => {
      username match {
        case Some(user) =>
          val passOpt = db.passwords.get(user)
          passOpt match {
            case None => false
            case Some(pass) => pass == password
          }
        case None => false
      }
    })

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => { db.passwords.get(username).contains(password) })

  def checkLoginOpt(userId: Int, password: String): DbReader[Boolean] = for {
    user <- findUsername(userId)
    login <- checkPasswordOpt(user, password)
  } yield login

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = for {
    user <- findUsername(userId)
    login <- user.map { username =>
      checkPassword(username, password)
    }.getOrElse( false.pure[DbReader] )
  } yield login

  val db = Db(
    Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    ),
    Map(
      "dade" -> "zerocool",
      "kate" -> "acidburn",
      "margo" -> "secret"
    )
  )

  println(checkLoginOpt(1, "zerocool").run(db))
  println(checkLoginOpt(4, "davinci").run(db))

  println(checkLogin(2, "acidbrun").run(db))
  println(checkLogin(3, "secret").run(db))
}
