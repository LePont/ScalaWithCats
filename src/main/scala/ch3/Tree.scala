package ch3

import cats.Functor

sealed trait Tree[+A]

final case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree {

  def Branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left,right)
  def Leaf[A](v: A): Tree[A] = Leaf(v)

  implicit val TreeFunctor: Functor[Tree] = {
    new Functor[Tree] {
      def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
        fa match {
          case Branch(l, r) => Branch(map(l)(f), map(r)(f))
          case Leaf(v) => Leaf(f(v))
        }
    }
  }
}