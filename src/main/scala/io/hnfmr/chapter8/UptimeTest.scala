package io.hnfmr.chapter8

import scala.language.higherKinds
import scala.concurrent.Future

import cats.Id
import cats.Applicative
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.functor._

object UptimeTest extends App {
  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  trait RealUptimeClient extends UptimeClient[Future] {
    def getUptime(hostname: String): Future[Int]
  }

  class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
    def getUptime(hostname: String): Id[Int] = hosts.getOrElse(hostname, 0)
  }

  // Context Bound
  class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] =
      hostnames.traverse(client.getUptime).map(_.sum)
  }

  def testTotalUptime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)

    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum

    assert(actual == expected)
  }

  testTotalUptime()
}
