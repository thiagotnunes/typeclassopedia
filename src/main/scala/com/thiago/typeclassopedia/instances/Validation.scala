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

  def append[EE >: E, AA >: A](a2: Validation[EE, AA])(implicit errorSemiGroup: SemiGroup[EE], successMonoid: Monoid[AA]): Validation[EE, AA] = {
    implicitly[Monoid[Validation[EE, AA]]].append(this, a2)
  }
}

object Validation {
  def pure[E, A](a: A): Validation[E, A] = {
    implicitly[Applicative[({type lambda[A] = Validation[E, A]})#lambda]].pure(a)
  }

  def `return`[E, A](a: A): Validation[E, A] = {
    implicitly[Monad[({type lambda[A] = Validation[E, A]})#lambda]].`return`(a)
  }

  def zero[E, A](implicit errorSemiGroup: SemiGroup[E], successMonoid: Monoid[A]): Validation[E, A] = {
    implicitly[Monoid[Validation[E, A]]].zero
  }

  implicit def ValidationMonoid[E, A](implicit errorSemiGroup: SemiGroup[E], successMonoid: Monoid[A]): Monoid[Validation[E, A]] =
    new Monoid[Validation[E, A]] {
      override def zero: Validation[E, A] = Success(successMonoid.zero)

      override def append(a1: Validation[E, A], a2: Validation[E, A]): Validation[E, A] = {
        (a1, a2) match {
          case (Success(v1), Success(v2)) => Success(successMonoid.append(v1, v2))
          case (failure @ Failure(_), Success(_)) => failure
          case (Success(_), failure @ Failure(_)) => failure
          case (failure1 @ Failure(v1), failure2 @ Failure(v2)) => Failure(errorSemiGroup.append(v1, v2))
        }
      }
    }

  implicit def ValidationMonad[E]: Monad[({type lambda[A] = Validation[E, A]})#lambda] =
    new Monad[({type lambda[A] = Validation[E, A]})#lambda] {
      override def fmap[A, B](fa: Validation[E, A])(f: (A) => B): Validation[E, B] = {
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

      override def bind[A, B](fa: Validation[E, A])(f: (A) => Validation[E, B]): Validation[E, B] = {
        fa match {
          case Success(a) => f(a)
          case failure @ Failure(_) => failure.asInstanceOf[Validation[E, B]]
        }
      }
    }

  case class Failure[E, A](error: E) extends Validation[E, A]

  case class Success[E, A](success: A) extends Validation[E, A]

}
