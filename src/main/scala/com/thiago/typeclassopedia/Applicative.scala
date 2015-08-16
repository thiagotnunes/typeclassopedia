package com.thiago.typeclassopedia

trait Applicative[F[_]] {
  def pure[A](a: A): F[A]
  def <*>[A, B](fa: F[A])(f: F[(A) => B]): F[B]
}
