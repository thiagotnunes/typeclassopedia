package com.thiago.typeclassopedia.instances

import com.thiago.typeclassopedia.definitions.Functor

sealed trait Validation[E, A]

object Validation {

  implicit class FunctionalValidation[E, A](m: Validation[E, A]) {
    def map[B](f: (A) => B): Validation[E, B] = {
      implicitly[Functor[({type lambda[A] = Validation[E, A]})#lambda]].map(m)(f)
    }
  }

  implicit def ValidationMonad[E]: Functor[({type lambda[A] = Validation[E, A]})#lambda] =
    new Functor[({type lambda[A] = Validation[E, A]})#lambda] {
      override def map[A, B](fa: Validation[E, A])(f: (A) => B): Validation[E, B] = {
        fa match {
          case failure @ Failure(_) => failure.asInstanceOf[Failure[E, B]]
          case Success(a) => Success(f(a))
        }
      }
    }

  case class Failure[E, A](error: E) extends Validation[E, A]

  case class Success[E, A](success: A) extends Validation[E, A]

}
