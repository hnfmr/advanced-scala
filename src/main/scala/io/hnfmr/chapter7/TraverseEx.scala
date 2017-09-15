package io.hnfmr.chapter7

import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object TraverseEx extends App {
  val hostnames = List(
    "www.google.com",
    "www.facebook.com"
  )

  def getUptime(hostname: String): Future[Int] =
    Future(hostname.length * 60)

  val allUptimes: Future[List[Int]] = Future.traverse(hostnames)(getUptime)
  println(
    Await.result(allUptimes, 1.second)
  )
}
