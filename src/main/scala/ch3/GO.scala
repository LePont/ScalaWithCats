package ch3

import cats.syntax.functor._
import Tree._
import ch1.PrintableInstances._
import ch1.PrintableSyntax._

object GO extends App{

  examples.mapOverFunctions()
  examples.mathExamples()

  val b1 = Box(true)
  b1.print

  val box = Box("dddd")
  box.print

  Tree.leaf(100).print
  Tree.branch(Leaf(10), Leaf(20)).map(_*2).print



}
