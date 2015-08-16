package com.thiago.typeclassopedia.definitions

trait Monad[F[_]] extends Applicative[F] {
  def `return`[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: (A) => F[B]): F[B]
}
