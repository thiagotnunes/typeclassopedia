package com.thiago.typeclassopedia

import scala.annotation.tailrec

trait Functor[F[_]] {
  def fmap[A, B](fa: F[A])(f: (A) => B): F[B]
}

object Functor {
  implicit def SeqFunctor: Functor[Seq] = new Functor[Seq] {
    override def fmap[A, B](fa: Seq[A])(f: (A) => B): Seq[B] = {
      @tailrec
      def iterate(fa: Seq[A], fb: Seq[B]): Seq[B] = {
        fa match {
          case Seq() => fb
          case head +: tail => iterate(tail, fb :+ f(head))
        }
      }

      iterate(fa, Seq.empty)
    }
  }

  implicit def MaybeFunctor: Functor[Maybe] = new Functor[Maybe] {
    override def fmap[A, B](fa: Maybe[A])(f: (A) => B): Maybe[B] = {
      fa match {
        case Just(a) => Just(f(a))
        case Empty() => Empty()
      }
    }
  }
}
