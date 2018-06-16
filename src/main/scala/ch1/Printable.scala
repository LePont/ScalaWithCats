package ch1

import ch3.{Branch, Leaf, Tree}


trait Printable[A] {
  self =>

  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] = {
    new Printable[B] {
      def format(value: B): String = {
          self.format(func(value))
      }
    }
  }
}

object PrintableInstances {
  implicit val stringPrintable = new Printable[String] {
    def format(value: String) = "\"" + value + "\""
  }

  implicit val intPrintable = new Printable[Int] {
    def format(value: Int) = value.toString
  }

  implicit val booleanPrintable = new Printable[Boolean] {
    def format(value: Boolean) = if(value) "yes" else "no"
  }

  implicit def treePrintable[A] = new Printable[Tree[A]] {
    def format(value: Tree[A]) = value match {
      case Leaf(number) => s"Leaf value: ${number.toString}"
      case Branch(l, r) => s"Branch(${format(l)} , ${format(r)})"
    }
  }
}

object Printable {
  def format[A](value: A)(implicit p: Printable[A]): String = {
    p.format(value)
  }

  def print[A](value: A)(implicit p: Printable[A]): Unit = {
    println(p.format(value))
  }
}
