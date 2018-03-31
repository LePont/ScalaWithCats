package ch2

import cats.Monoid
import cats.instances.all._

import scala.util.Random

object GO extends App {

  def makeList():List[Int] = {
    val v = List.fill(6)(Random.nextInt(300))
    println(s"Full list: $v")
    v
  }
  def makeOptList():List[Option[Int]] = {
    val v = makeList().map(s => if(s % 2 == 0) Some(s) else None)
    println(s"Full Opt List $v")
    v
  }

  def makeOrderList(): List[Order] = {
    List.fill(5)(Order(Random.nextInt(500).toDouble, Random.nextInt(10).toDouble))
  }

  def makeBankAccountList(): List[BankAccount] = {
    List.fill(50)(BankAccount(Random.nextInt().toDouble, Math.abs(Random.nextInt()).toDouble))
  }

  println(SuperAdder.add(makeList()))
  println(SuperAdder.altAdd(makeList()))
  println(SuperAdder.monoidAdd(makeList()))

  println(SuperAdder.monoidAdd(makeOptList()))
  println(SuperAdder.monoidAdd2(makeOptList()))

  println("Orders:")

  val orders = makeOrderList()
  println(orders)

  // Not really what an order is shouldn't sum quantity and cost if looking for total order value
  println(SuperAdder.monoidAdd(orders))

  //fix to add total cost to case class and sum it but don't need custom Monoid then.

  val valueList = orders.map(o => o.totalCost)
  println(valueList)
  println(SuperAdder.monoidAdd(valueList))

  /*
  thinking about different example for adder
  Imagine customer bank account. Each accounr has a current balance and a saving.
  we want to calculate the value of a banks current holdings and savings holding.
   */

  println("Bank Account idea")
  val accounts = makeBankAccountList()
  println(accounts)
  println(SuperAdder.monoidAdd(accounts))



}