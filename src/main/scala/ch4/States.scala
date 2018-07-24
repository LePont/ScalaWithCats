package ch4

import cats.data.State
import cats.syntax.applicative._
object States {

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = {
    sym match {
      case "+" => dealWithOperator(_ + _)
      case "-" => dealWithOperator(_ - _)
      case "*" => dealWithOperator(_ * _)
      case "/" => dealWithOperator(_ / _)
      case number => dealWithNumber(number.toInt)
    }
  }

  def evalAll(input: List[String]): CalcState[Int] = {
    input.foldLeft(0.pure[CalcState]){(a, b) =>
      a.flatMap(_ => evalOne(b))
    }
  }

  def evalInput(input: String): Int = {
    evalAll(input.split(" ").toList).runA(Nil).value
  }

  private def dealWithNumber(number: Int): CalcState[Int] = {
    State[List[Int], Int] {
      stack => (number :: stack, number)
    }
  }

  private def dealWithOperator(func: (Int, Int) => Int): CalcState[Int] = {
    State[List[Int], Int] {
      case a :: b :: tail  =>
        val result = func(a, b)
        (result :: tail , result)
      case _ => sys.error("NOnONONONONNO")
    }
  }
}
