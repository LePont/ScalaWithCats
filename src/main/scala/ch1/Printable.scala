package ch1


trait Printable[A] {
  self =>

  def format(value:A): String

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
}

object Printable {
  def format[A](value: A)(implicit p: Printable[A]): String = {
    p.format(value)
  }

  def print[A](value: A)(implicit p: Printable[A]): Unit = {
    println(p.format(value))
  }
}
