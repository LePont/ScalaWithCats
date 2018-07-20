package ch4

import cats.data.State

object States {

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = {
    sym match {
      case number => dealWithNumber(number.toInt)
      case "+" => dealWithOperator(_ + _)
    }

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
