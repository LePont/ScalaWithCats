package ch3

import cats.syntax.functor._
import Tree._

object GO extends App{

  examples.mapOverFunctions()
  examples.mathExamples()

  Tree.Branch(Leaf(10), Leaf(20)).map(_ * 2)

}
