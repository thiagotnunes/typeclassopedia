package com.thiago.typeclassopedia.instances

import com.thiago.typeclassopedia.definitions.Monoid

object IntExtensions {
  def zero: Int = {
    implicitly[Monoid[Int]].zero
  }

  implicit class FunctionalInt(i: Int) {
    def append(a2: Int): Int = {
      implicitly[Monoid[Int]].append(i, a2)
    }
  }

  implicit val IntMonoid: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 0

    override def append(a1: Int, a2: Int): Int = a1 + a2
  }
}
