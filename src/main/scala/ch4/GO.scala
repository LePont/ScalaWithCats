package ch4

import cats.data.Writer
import cats.{Eval, Id, Monad}

object GO extends App {

  // Identity playground
  val f = List(1, 2, 3): Id[List[Int]]
  f.map(f => f + 1)

  val a = Monad[Id].pure(List(1, 2, 3))
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


  //EVAL

  def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail => Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil => acc
    }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc)) { (a, b) =>
      b.map(fn(a, _))
    }.value


}
