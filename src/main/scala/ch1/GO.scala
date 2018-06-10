package ch1

import PrintableSyntax._
import PrintableInstances._
import cats.implicits._

object GO extends App {

  val cat = Cat("Buster", 2, "Blue")
  cat.print
  "fvbb".print

  val cat1 = Cat("Bluebell", 3, "Ginger")
  println(cat1.show)

  val cat11 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")

  println(cat11 === cat2)
  println(cat11 =!= cat2)

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  println(optionCat1 === optionCat2)
  println(optionCat1 =!= optionCat2)

  false.print
}
