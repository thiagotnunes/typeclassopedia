package com.thiago.typeclassopedia.instances

import com.thiago.typeclassopedia.definitions.{Applicative, Functor, Monad}

sealed trait Validation[+E, +A]

object Validation {

  def pure[E, A](a: A): Validation[E, A] = {
    implicitly[Applicative[({type lambda[A] = Validation[E, A]})#lambda]].pure(a)
  }

  def `return`[E, A](a: A): Validation[E, A] = {
    implicitly[Monad[({type lambda[A] = Validation[E, A]})#lambda]].`return`(a)
  }

  implicit class FunctionalValidation[E, A](m: Validation[E, A]) {
    def map[B](f: (A) => B): Validation[E, B] = {
      implicitly[Functor[({type lambda[A] = Validation[E, A]})#lambda]].map(m)(f)
    }

    def ap[B](f: Validation[E, (A) => B]): Validation[E, B] = {
      implicitly[Applicative[({type lambda[A] = Validation[E, A]})#lambda]].ap(m)(f)
    }

    def flatMap[B](f: (A) => Validation[E, B]): Validation[E, B] = {
      implicitly[Monad[({type lambda[A] = Validation[E, A]})#lambda]].flatMap(m)(f)
    }
  }

  implicit def ValidationMonad[E]: Monad[({type lambda[A] = Validation[E, A]})#lambda] =
    new Monad[({type lambda[A] = Validation[E, A]})#lambda] {
      override def map[A, B](fa: Validation[E, A])(f: (A) => B): Validation[E, B] = {
        fa match {
          case failure @ Failure(_) => failure.asInstanceOf[Failure[E, B]]
          case Success(a) => Success(f(a))
        }
      }

      override def pure[A](a: A): Validation[E, A] = Success(a)

      override def ap[A, B](fa: Validation[E, A])(f: Validation[E, (A) => B]): Validation[E, B] = {
        (fa, f) match {
          case (Success(a), Success(fab)) => pure(fab(a))
          case (Success(_), failure @ Failure(error)) => failure.asInstanceOf[Validation[E, B]]
          case (failure @ Failure(error), _) => failure.asInstanceOf[Validation[E, B]]
        }
      }

      override def `return`[A](a: A): Validation[E, A] = pure(a)

      override def flatMap[A, B](fa: Validation[E, A])(f: (A) => Validation[E, B]): Validation[E, B] = {
        fa match {
          case Success(a) => f(a)
          case failure @ Failure(_) => failure.asInstanceOf[Validation[E, B]]
        }
      }
    }

  case class Failure[E, A](error: E) extends Validation[E, A]

  case class Success[E, A](success: A) extends Validation[E, A]

}
