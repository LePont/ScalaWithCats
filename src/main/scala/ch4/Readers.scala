package ch4

import cats.data.Reader
import cats.syntax.applicative._

object Readers {

  type DbReader[A] = Reader[Db, A]

  private def findUserName(userId: Int): DbReader[Option[String]] =
    Reader(Db => Db.userNames.get(userId))

  private def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(Db => Db.password.get(username).exists(_ equals password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      maybeUsername <- findUserName(userId)
      passwordOk    <- maybeUsername.map(name =>
        checkPassword(name, password)).getOrElse(false.pure[DbReader])
    }yield passwordOk
}

case class Db(userNames: Map[Int, String], password: Map[String, String])
