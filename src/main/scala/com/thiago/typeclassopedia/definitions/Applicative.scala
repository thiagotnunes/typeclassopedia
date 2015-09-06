package com.thiago.typeclassopedia.definitions

trait Applicative[T[_]] extends Pointed[T] {
  def ap[A, B](fa: T[A])(f: T[(A) => B]): T[B]
}
