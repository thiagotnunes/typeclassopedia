package com.thiago.typeclassopedia.definitions

trait Applicative[T[_]] extends Functor[T] {
  def pure[A](a: A): T[A]
  def ap[A, B](fa: T[A])(f: T[(A) => B]): T[B]
}
