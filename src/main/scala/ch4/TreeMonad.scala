package ch4

import cats.Monad

object TreeMonad {

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {

    def pure[A](x: A): Tree[A] = Leaf(x)

    def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = {
      fa match {
        case Branch(l, r) => Branch(flatMap(l)(f),flatMap(r)(f))
        case Leaf(a) =>  f(a)
      }
    }

    def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      f(a) match {
        case Branch(l, r) =>
          Branch(
            flatMap(l){
              case Left(v) => tailRecM(v)(f)
              case Right(v) => pure(v)
            },
            flatMap(r){
              case Left(v) => tailRecM(v)(f)
              case Right(v) => pure(v)
            }
          )
        case Leaf(Left(v))  => tailRecM(v)(f)
        case Leaf(Right(v)) => Leaf(v)
      }
    }
  }

}
