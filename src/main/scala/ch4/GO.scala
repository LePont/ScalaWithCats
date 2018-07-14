package ch4

import cats.Id
import cats.Monad

object GO extends App{

  // Identity playground
  val f = List(1,2,3): Id[List[Int]]
  f.map(f => f + 1)

  val a = Monad[Id].pure(List(1,2,3))
  val b = Monad[Id].flatMap(a)(rr => rr.map(_ + 1))

  println(a)
  println(b)

  val d: Id[List[Int]] = for {
    x <- a
    y <- b
  } yield x + y

  println(d)

  // map and flatMap are the same mindblown :)
  // Id[A] is just A


}
