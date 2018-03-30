package ch2

object MonoidDefs {

  trait SemiGroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends SemiGroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit monoid: Monoid[A]) = monoid
  }

}
