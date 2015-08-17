package com.thiago.typeclassopedia.definitions

trait Monoid[T] extends Semigroup[T] {
  def zero: T
}
