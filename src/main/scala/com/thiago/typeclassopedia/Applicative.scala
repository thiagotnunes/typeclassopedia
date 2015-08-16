package com.thiago.typeclassopedia

trait Applicative[F[_]] {
  def pure[A](a: A): F[A]
  def <*>[A, B](fa: F[A])(f: F[(A) => B]): F[B]
}

object Applicative {
  implicit def MaybeApplicative: Applicative[Maybe] = new Applicative[Maybe] {
    override def pure[A](a: A): Maybe[A] = {
      if(a != null) {
        Just(a)
      } else {
        Empty()
      }
    }

    override def <*>[A, B](fa: Maybe[A])(f: Maybe[(A) => B]): Maybe[B] = {
      (fa, f) match {
        case (Just(a), Just(fab)) => pure(fab(a))
        case _ => Empty()
      }
    }
  }
}