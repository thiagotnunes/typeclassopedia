package com.thiago.typeclassopedia.definitions

trait Monoid[T] extends SemiGroup[T] {
  def zero: T
}
