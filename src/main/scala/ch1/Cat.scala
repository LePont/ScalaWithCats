package ch1

import cats._
import cats.implicits._

final case class Cat(name: String, age: Int, colour: String)

object Cat {
  implicit val catPrintable = new Printable[Cat] {
    def format(cat: Cat) = s"${cat.name} is a ${cat.age} year-old ${cat.colour} cat."
  }

  implicit val catShow: Show[Cat] = Show.show(cat => s"${cat.name} is a ${cat.age} year-old ${cat.colour} cat.")

  implicit val catEq: Eq[Cat] =
    Eq.instance[Cat] {
      (cat1, cat2) =>
        (cat1.age === cat2.age) && (cat1.colour === cat2.colour) && (cat1.name === cat2.name)
    }
}