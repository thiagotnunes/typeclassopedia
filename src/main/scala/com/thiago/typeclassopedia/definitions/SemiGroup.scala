package com.thiago.typeclassopedia.definitions

trait SemiGroup[T] {
  def append(a1: T, a2: T): T
}
