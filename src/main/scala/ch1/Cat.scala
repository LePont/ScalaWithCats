package ch1

final case class Cat(name: String, age: Int, colour: String)

object Cat {
  implicit val catPrintable = new Printable[Cat] {
    def format(cat: Cat) = s"${cat.name} is a ${cat.age} year-old ${cat.colour} cat."
  }
}