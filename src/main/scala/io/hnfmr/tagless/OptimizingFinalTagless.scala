package io.hnfmr.tagless

import scala.language.higherKinds
import cats._
import cats.implicits._
import cats.data.Const

object OptimizingFinalTagless extends App {

  trait KVStore[F[_]] {
    def get(key: String): F[Option[String]]
    def put(key: String, a: String): F[Unit]
  }

  def program0[M[_]: FlatMap, F[_]](a: String)(K: KVStore[M])(implicit P: Parallel[M, F]) =
    for {
      _ <- K.put("A", a)
      x <- (K.get("B"), K.get("C")).parMapN(_ |+| _)
      _ <- K.put("X", x.getOrElse("-"))
    } yield x


  // This is essentially a Semigroupal Tuple4 mapped into F
  def program1[F[_] : Apply](K: KVStore[F]): F[List[String]] =
    (K.get("Cats"), K.get("Dogs"), K.put("Mice", "42"), K.get("Cats"))
      .mapN((f, s, _, t) => List(f, s, t).flatten)

  def analysisInterpreter: KVStore[Const[(Set[String], Map[String, String]), ?]] =
    new KVStore[Const[(Set[String], Map[String, String]), ?]] {
      def get(key: String) = Const((Set(key), Map.empty))
      def put(key: String, a: String) = Const((Set.empty, Map(key -> a)))
    }

  // Here the F[_] is Const[(Set[String], Map[String, String]), ?]
  // The result of Tuple4 Semigroup4 is Set("Cats", "Dogs"), due to analysisInterpreter having KVStore with F[_] defined above
  // But this is all done within the F[_] context and getConst
  println(program1(analysisInterpreter).getConst)

  def optimizedProgram[F[_]: Applicative](K: KVStore[F]): F[List[String]] = {
    val (gets, puts) = program1(analysisInterpreter).getConst

    puts.toList.traverse { case (k, v) => K.put(k, v) } *> gets.toList.traverse(K.get).map(_.flatten)
  }

  def monadicProgram[F[_] : Monad](K: KVStore[F]): F[Unit] = for {
    list <- optimizedProgram(K)
    _ <- K.put("Birds", list.headOption.getOrElse("128"))
  } yield ()
}