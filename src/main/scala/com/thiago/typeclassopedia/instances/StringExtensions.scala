package com.thiago.typeclassopedia.instances

import com.thiago.typeclassopedia.definitions.Monoid

object StringExtensions {
  def zero: String = {
    implicitly[Monoid[String]].zero
  }

  implicit class FunctionalString(i: String) {
    def append(a2: String): String = {
      implicitly[Monoid[String]].append(i, a2)
    }
  }

  implicit def StringMonoid: Monoid[String] = new Monoid[String] {
    override def zero: String = ""

    override def append(a1: String, a2: String): String = a1 + a2
  }
}
