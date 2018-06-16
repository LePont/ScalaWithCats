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

  Tree.leaf(true).print
  Tree.branch(Leaf(10), Leaf(20)).map(_*2).print

  intCodec.encode(123).print
  intCodec.decode("34562").print

  doubleCodec.decode("123.34").print

  boxCodec[Int].encode(Box(123)).print
  boxCodec[Double].decode("123.34").print
}


