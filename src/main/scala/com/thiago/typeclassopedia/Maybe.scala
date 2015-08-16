package com.thiago.typeclassopedia

sealed trait Maybe[A]

object Maybe {
  def pure[A](a: A): Maybe[A] = {
    implicitly[Applicative[Maybe]].pure(a)
  }

  implicit class FunctionalMaybe[A](m: Maybe[A]) {
    def fmap[B](f: (A) => B): Maybe[B] = {
      implicitly[Functor[Maybe]].fmap(m)(f)
    }

    def <*>[B](f: Maybe[(A) => B]): Maybe[B] = {
      implicitly[Applicative[Maybe]].<*>(m)(f)
    }
  }

  implicit def MaybeApplicative: Applicative[Maybe] = new Applicative[Maybe] {
    override def fmap[A, B](fa: Maybe[A])(f: (A) => B): Maybe[B] = {
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

    override def <*>[A, B](fa: Maybe[A])(f: Maybe[(A) => B]): Maybe[B] = {
      (fa, f) match {
        case (Just(a), Just(fab)) => pure(fab(a))
        case _ => Empty()
      }
    }
  }
}

case class Just[A](a: A) extends Maybe[A]
case class Empty[A]() extends Maybe[A]
