package ch1

import ch3._


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

  implicit def genericPrintable[A] = new Printable[A] {
    override def format(value: A): String = value.toString
  }

  implicit val stringCodec: Codec[String] = {
    new Codec[String] {
      def encode(value: String): String = value

      def decode(value: String): String = value
    }
  }

  implicit val intCodec: Codec[Int] = {
    stringCodec.imap(_.toInt, _.toString)
  }

  implicit val booleanCodec: Codec[Boolean] = {
    stringCodec.imap(_.toBoolean, _.toString)
  }

  implicit val doubleCodec: Codec[Double] = {
    stringCodec.imap(_.toDouble, _.toString)
  }

  implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] = {
    c.imap[Box[A]](Box(_), _.value)
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
