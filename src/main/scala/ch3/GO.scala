package ch3

object GO extends App{

  examples.mapOverFunctions()
  examples.mathExamples()

  val t = Branch(Leaf(10), Leaf(20))
  t.map(_ * 2)

}
