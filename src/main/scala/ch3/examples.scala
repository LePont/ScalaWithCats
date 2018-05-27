package ch3

import cats.Functor
import cats.instances.function._
import cats.syntax.functor._

object examples {

  def mapOverFunctions() = {
    val f1 = (a: Int) => a + 1
    val f2 = (a: Int) => a * 2
    val f3 = (a: Int) => a + "!"
    val f4 = f1.map(f2).map(f3)

    println(f4(123))
  }


  private def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] =
    start.map(n => n+10*2)


  def mathExamples() = {

    import cats.instances.option._
    import cats.instances.list._

    println(doMath(Option(20)))
    println(doMath(List(1,2,3)))
  }

}
