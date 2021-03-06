package com.thiago.typeclassopedia.definitions

trait Monad[T[_]] extends Applicative[T] {
  def bind[A, B](fa: T[A])(f: (A) => T[B]): T[B]
}
