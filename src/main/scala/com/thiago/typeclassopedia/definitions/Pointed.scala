package com.thiago.typeclassopedia.definitions

trait Pointed[T[_]] extends Functor[T] {
  def point[A](a: A): T[A]
}
