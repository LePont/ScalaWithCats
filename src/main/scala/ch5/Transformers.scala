package ch5

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.flatMap._
import scala.concurrent.duration._
import cats.syntax.applicative._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object Transformers {

  val powerLevels = Map(
    "Jazz"      -> 6,
    "Bumblebee" -> 8,
    "Hot Rod"   -> 10
  )

  type Response[A] = EitherT[Future, String, A]

  def getPowerLevel(autobot: String): Response[Int] = {
      powerLevels.get(autobot) match {
        case Some(level) => EitherT.right(Future(level))
        case None        => EitherT.left(Future(s"$autobot unreacheable"))
      }
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
    for {
      powerA <- getPowerLevel(ally1)
      powerB <- getPowerLevel(ally2)
    } yield (powerA + powerB) > 15
  }

  def tacticalReport(ally1: String, ally2: String): String = {
    val response = canSpecialMove(ally1, ally2).value
    Await.result(response, 10.second) match {
      case Left(e) =>  s"Error: $e"
      case Right(false) => s"$ally1 and $ally2 not enough power"
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
    }
  }

}
