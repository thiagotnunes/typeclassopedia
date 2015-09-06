package com.thiago.typeclassopedia.instances

import com.thiago.typeclassopedia.definitions._

sealed trait Validation[+E, +A] {
  def map[EE >: E, B](f: (A) => B): Validation[EE, B] = {
    implicitly[Functor[({type lambda[A] = Validation[EE, A]})#lambda]].fmap(this)(f)
  }

  def ap[EE >: E, B](f: Validation[EE, (A) => B]): Validation[EE, B] = {
    implicitly[Applicative[({type lambda[A] = Validation[EE, A]})#lambda]].ap(this)(f)
  }

  def flatMap[EE >: E, B](f: (A) => Validation[EE, B]): Validation[EE, B] = {
    implicitly[Monad[({type lambda[A] = Validation[EE, A]})#lambda]].bind(this)(f)
  }

  def append[EE >: E : Semigroup, AA >: A : Monoid](a2: Validation[EE, AA]): Validation[EE, AA] = {
    implicitly[Monoid[Validation[EE, AA]]].append(this, a2)
  }
}

object Validation {
  def point[E, A](a: A): Validation[E, A] = {
    implicitly[Applicative[({type lambda[A] = Validation[E, A]})#lambda]].point(a)
  }

  def zero[E : Semigroup, A : Monoid]: Validation[E, A] = {
    implicitly[Monoid[Validation[E, A]]].zero
  }

  implicit def ValidationMonoid[E : Semigroup, A : Monoid]: Monoid[Validation[E, A]] =
    new Monoid[Validation[E, A]] {
      override def zero: Validation[E, A] = Success(implicitly[Monoid[A]].zero)

      override def append(a1: Validation[E, A], a2: Validation[E, A]): Validation[E, A] = {
        (a1, a2) match {
          case (Success(v1), Success(v2)) => Success(implicitly[Monoid[A]].append(v1, v2))
          case (failure @ Failure(_), Success(_)) => failure
          case (Success(_), failure @ Failure(_)) => failure
          case (failure1 @ Failure(v1), failure2 @ Failure(v2)) => Failure(implicitly[Semigroup[E]].append(v1, v2))
        }
      }
    }

  implicit def ValidationMonad[E]: Monad[({type lambda[A] = Validation[E, A]})#lambda] =
    new Monad[({type lambda[A] = Validation[E, A]})#lambda] {
      override def fmap[A, B](fa: Validation[E, A])(f: (A) => B): Validation[E, B] = {
        fa match {
          case Failure(error) => Failure(error)
          case Success(a) => Success(f(a))
        }
      }

      override def point[A](a: A): Validation[E, A] = Success(a)

      override def ap[A, B](fa: Validation[E, A])(f: Validation[E, (A) => B]): Validation[E, B] = {
        (fa, f) match {
          case (Success(a), Success(fab)) => point(fab(a))
          case (Success(_), Failure(error)) => Failure(error)
          case (Failure(error), _) => Failure(error)
        }
      }

      override def bind[A, B](fa: Validation[E, A])(f: (A) => Validation[E, B]): Validation[E, B] = {
        fa match {
          case Success(a) => f(a)
          case Failure(error) => Failure(error)
        }
      }
    }

  case class Failure[E, A](error: E) extends Validation[E, A]

  case class Success[E, A](success: A) extends Validation[E, A]

}
