package com.thiago.typeclassopedia.instances

import com.thiago.typeclassopedia.definitions.{Monad, Functor, Applicative}

sealed trait Maybe[A]

object Maybe {
  def pure[A](a: A): Maybe[A] = {
    implicitly[Applicative[Maybe]].pure(a)
  }

  def `return`[A](a: A): Maybe[A] = {
    pure(a)
  }

  implicit class FunctionalMaybe[A](m: Maybe[A]) {
    def map[B](f: (A) => B): Maybe[B] = {
      implicitly[Functor[Maybe]].map(m)(f)
    }

    def <*>[B](f: Maybe[(A) => B]): Maybe[B] = {
      implicitly[Applicative[Maybe]].ap(m)(f)
    }

    def flatMap[B](f: (A) => Maybe[B]): Maybe[B] = {
      implicitly[Monad[Maybe]].flatMap(m)(f)
    }
  }

  implicit def MaybeMonad: Monad[Maybe] = new Monad[Maybe] {
    override def map[A, B](fa: Maybe[A])(f: (A) => B): Maybe[B] = {
      fa match {
        case Just(a) => Just(f(a))
        case Empty() => Empty()
      }
    }

    override def pure[A](a: A): Maybe[A] = {
      if(a != null) {
        Just(a)
      } else {
        Empty()
      }
    }

    override def ap[A, B](fa: Maybe[A])(f: Maybe[(A) => B]): Maybe[B] = {
      (fa, f) match {
        case (Just(a), Just(fab)) => pure(fab(a))
        case _ => Empty()
      }
    }

    override def `return`[A](a: A): Maybe[A] = {
      pure(a)
    }

    override def flatMap[A, B](fa: Maybe[A])(f: (A) => Maybe[B]): Maybe[B] = {
      fa match {
        case Just(a) => f(a)
        case Empty() => Empty()
      }
    }
  }

  case class Just[A](a: A) extends Maybe[A]
  case class Empty[A]() extends Maybe[A]
}
