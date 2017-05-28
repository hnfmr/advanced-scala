package io.hnfmr.chapter4

object EitherTut extends App {
  // Either in Scala 2.12 is right-biased
  val either0 = Right(123)
  val either1 = either0.flatMap(x => Right(x + 1))
  val either2 = either0.flatMap(x => Right(x + 2))

  for {
    a <- either1
    b <- either2
  } yield a + b

}
