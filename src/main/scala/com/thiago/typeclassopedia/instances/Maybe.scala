package com.thiago.typeclassopedia.instances

import com.thiago.typeclassopedia.definitions._

sealed trait Maybe[+A] {
  def map[B](f: (A) => B): Maybe[B] = {
    implicitly[Functor[Maybe]].fmap(this)(f)
  }

  def ap[B](f: Maybe[(A) => B]): Maybe[B] = {
    implicitly[Applicative[Maybe]].ap(this)(f)
  }

  def flatMap[B](f: (A) => Maybe[B]): Maybe[B] = {
    implicitly[Monad[Maybe]].bind(this)(f)
  }

  def append[AA >: A : Monoid](a2: Maybe[AA]): Maybe[AA] = {
    implicitly[Monoid[Maybe[AA]]].append(this, a2)
  }
}

object Maybe {
  def pure[A](a: A): Maybe[A] = {
    implicitly[Applicative[Maybe]].pure(a)
  }

  def `return`[A](a: A): Maybe[A] = {
    implicitly[Monad[Maybe]].`return`(a)
  }

  def zero[A : Monoid]: Maybe[A] = {
    implicitly[Monoid[Maybe[A]]].zero
  }

  implicit def MaybeMonoid[A : Monoid]: Monoid[Maybe[A]] = new Monoid[Maybe[A]] {
    override def zero: Maybe[A] = {
      Empty
    }

    override def append(a1: Maybe[A], a2: Maybe[A]): Maybe[A] = {
      (a1, a2) match {
        case (Just(v1), Just(v2)) => Just(implicitly[Monoid[A]].append(v1, v2))
        case (Just(_), Empty) => a1
        case (Empty, Just(_)) => a2
        case (Empty, Empty) => Empty
      }
    }
  }

  implicit val MaybeMonad: Monad[Maybe] = new Monad[Maybe] {
    override def fmap[A, B](fa: Maybe[A])(f: (A) => B): Maybe[B] = {
      fa match {
        case Just(a) => Just(f(a))
        case Empty => Empty
      }
    }

    override def pure[A](a: A): Maybe[A] = {
      if(a != null) {
        Just(a)
      } else {
        Empty
      }
    }

    override def ap[A, B](fa: Maybe[A])(f: Maybe[(A) => B]): Maybe[B] = {
      (fa, f) match {
        case (Just(a), Just(fab)) => pure(fab(a))
        case _ => Empty
      }
    }

    override def `return`[A](a: A): Maybe[A] = {
      pure(a)
    }

    override def bind[A, B](fa: Maybe[A])(f: (A) => Maybe[B]): Maybe[B] = {
      fa match {
        case Just(a) => f(a)
        case Empty => Empty
      }
    }
  }

  case class Just[A](a: A) extends Maybe[A]
  case object Empty extends Maybe[Nothing]
}
