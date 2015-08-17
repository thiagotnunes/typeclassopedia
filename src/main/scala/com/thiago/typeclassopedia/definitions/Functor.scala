package com.thiago.typeclassopedia.definitions

trait Functor[T[_]] {
  def fmap[A, B](fa: T[A])(f: (A) => B): T[B]
}
