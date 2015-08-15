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

  implicit def OptionFunctor: Functor[Option] = new Functor[Option] {
    override def fmap[A, B](fa: Option[A])(f: (A) => B): Option[B] = {
      fa match {
        case Some(a) => Option(f(a))
        case None => None
      }
    }
  }
}
