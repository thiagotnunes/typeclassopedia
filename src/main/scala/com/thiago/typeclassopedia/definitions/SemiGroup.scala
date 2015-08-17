package com.thiago.typeclassopedia.definitions

trait Semigroup[T] {
  def append(a1: T, a2: T): T
}
