package ch2

import MonoidDefs._

class SetMonoids {

  implicit def setUnionMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def combine(a: Set[A], b: Set[A]) = a union b
    def empty = Set.empty[A]
  }

  implicit def setIntersect[A]: SemiGroup[Set[A]] = new SemiGroup[Set[A]] {
    def combine(a: Set[A], b: Set[A]) = a intersect b
  }

  implicit def SymetricDifference[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def combine(a: Set[A], b: Set[A]) = (a diff b) union (b diff a)
    def empty = Set.empty[A]
  }


}
