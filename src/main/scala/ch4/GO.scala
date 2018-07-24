package ch4

import cats.data.Writer
import cats.{Eval, Id, Monad}

object GO extends App {

  // Identity playground
  println("**** IDENTITY ****")

  val f = List(1, 2, 3): Id[List[Int]]
  f.map(f => f + 1)

  val a = Monad[Id].pure(List(1, 2, 3))
  val b = Monad[Id].flatMap(a)(rr => rr.map(_ + 1))

  println(a)
  println(b)

  val d: Id[List[Int]] = for {
    x <- a
    y <- b
  } yield x + y

  println(d)

  // map and flatMap are the same mindblown :)
  // Id[A] is just A

  //EVAL
  println("**** EVAL ****")

  def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail => Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil => acc
    }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc)) { (a, b) =>
      b.map(fn(a, _))
    }.value


  //Writer

  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  import cats.syntax.applicative._
  import cats.syntax.writer._
  import cats.instances.vector._
  println("**** WRITERS ****")

  val Vector((log1, ans1), (log2 ,an2)) = Await.result(Future.sequence(Vector(
    Future(factorial(3).run),
    Future(factorial(7).run)
  )), 5.seconds)

  println(log1)
  println(ans1)

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  type Logged[A] = Writer[Vector[String], A]

  def factorial(n: Int): Logged[Int] = {
    for {
        ans <- if (n == 0) {
          1.pure[Logged]
        } else {
          slowly(factorial(n -1).map(_ * n))
        }
        _ <- Vector(s"fact $n $ans").tell
      } yield ans
    }

// Reader

  import ch4.Readers._

  println("**** READERS ****")
  val users = Map(
    1 -> "d",
    2 -> "c",
    3 -> "r"
  )

  val passwords = Map(
    "d" -> "go",
    "c" -> "gadget",
    "r" -> "train"
  )

  val db = Db(users, passwords)
  println(checkLogin(1, "go").run(db))
  println(checkLogin(2, "fail").run(db))

  // STATE
  import States._
  println("**** STATES ****")

  println(evalOne("4").runA(Nil).value)
  val program = for {
    _ <- evalOne("42")
    _ <- evalOne("4")
    ans <- evalOne("+")
  } yield ans

  println(program.runA(Nil).value)

  val programAll = evalAll(List("1", "2", "+", "3", "*"))
  println(programAll.runA(Nil).value)

  println(evalInput("2 4 * 12 6 + 1 2 + 20 1 - 3 6 + *"))
}