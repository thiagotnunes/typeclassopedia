package com.thiago.typeclassopedia

trait Applicative[F[_]] {
  def pure[A](a: A): F[A]
  def <*>[A, B](fa: F[A])(f: F[(A) => B]): F[B]
}

object Applicative {
  implicit def OptionApplicative: Applicative[Option] = new Applicative[Option] {
    override def pure[A](a: A): Option[A] = {
      if(a != null) {
        Some(a)
      } else {
        None
      }
    }

    override def <*>[A, B](fa: Option[A])(f: Option[(A) => B]): Option[B] = {
      (fa, f) match {
        case (Some(a), Some(fab)) => pure(fab(a))
        case _ => None
      }
    }
  }
}