package com.thiago.typeclassopedia

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def <*>[A, B](fa: F[A])(f: F[(A) => B]): F[B]
}
