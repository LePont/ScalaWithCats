package ch3

trait Codec[A] {
  def encode(value: A): String

  def decode(value: String): A

  def imap[B](dec: A => B, enc: B => A): Codec[B] = {
    val self = this
    new Codec[B] {
      def encode(value: B): String =
        self.encode(enc(value))

      def decode(value: String): B =
        dec(self.decode(value))
    }
  }
}

object Codecs {

  implicit val stringCodec: Codec[String] = {
    new Codec[String] {
      def encode(value: String): String = value

      def decode(value: String): String = value
    }
  }

  implicit val IntCodec: Codec[Int] = {
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