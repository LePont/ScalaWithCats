package ch3

import ch1.Printable

final case class Box[A](value: A)

object Box {
  implicit def boxPrintable[A](implicit p: Printable[A]) =
    p.contramap[Box[A]](_.value)
}