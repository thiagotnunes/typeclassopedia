package com.thiago.typeclassopedia.definitions

trait Monad[T[_]] extends Applicative[T] {
  def `return`[A](a: A): T[A]
  def flatMap[A, B](fa: T[A])(f: (A) => T[B]): T[B]
}
