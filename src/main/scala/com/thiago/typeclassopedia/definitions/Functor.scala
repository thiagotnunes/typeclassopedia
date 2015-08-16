package com.thiago.typeclassopedia.definitions

trait Functor[T[_]] {
  def map[A, B](fa: T[A])(f: (A) => B): T[B]
}
