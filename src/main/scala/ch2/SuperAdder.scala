package ch2

import cats.Monoid
import cats.instances.all._
import cats.syntax.semigroup._
import ch2.MonoidDefs.SemiGroup

object SuperAdder {

  def add(items: List[Int]): Int = {
    items.sum
  }

  def altAdd(items: List[Int]): Int = {
    items.foldLeft(0)(_ + _)
  }

  def monoidAdd[A: Monoid](items: List[A]): A = {
    items.foldLeft(Monoid[A].empty)(_ |+| _)
  }

  //pass implicit version
  def monoidAdd2[A](items: List[A])(implicit monoid: Monoid[A]): A = {
    items.foldLeft(monoid.empty)(_ |+| _)
  }
}

case class Order(itemCost: Double, quantity: Double) {
  //small tweak to example
  val totalCost: Double = itemCost * quantity
}

object Order {
  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    def combine(a: Order, b: Order) = {
      Order(
        a.itemCost + b.itemCost,
        a.quantity + b.quantity
      )
    }
    def empty() = Order(0,0)
  }
}


case class BankAccount(currentAccountBalance: Double, savingAccountBalance: Double)
object BankAccount{
  implicit val bankAccountMonoid: Monoid[BankAccount] = new Monoid[BankAccount] {
    def combine(ac1: BankAccount, ac2: BankAccount): BankAccount = {
      BankAccount(
        ac1.currentAccountBalance + ac2.currentAccountBalance,
        ac1.savingAccountBalance + ac2.savingAccountBalance
      )
    }

    def empty() = BankAccount(0,0)
  }
}