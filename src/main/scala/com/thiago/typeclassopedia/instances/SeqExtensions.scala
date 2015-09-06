package com.thiago.typeclassopedia.instances

import com.thiago.typeclassopedia.definitions.{Monad, Monoid}

import scala.annotation.tailrec

object SeqExtensions {
  def point[A](a: A): Seq[A] = {
    implicitly[Monad[Seq]].point(a)
  }

  def zero[A : Monoid]: Seq[A] = {
    implicitly[Monoid[Seq[A]]].zero
  }

  implicit class FunctionalSeq[A](seq: Seq[A]) {
    def fmap[B](f: (A) => B): Seq[B] = {
      implicitly[Monad[Seq]].fmap(seq)(f)
    }

    def ap[B](f: Seq[(A) => B]): Seq[B] = {
      implicitly[Monad[Seq]].ap(seq)(f)
    }

    def bind[B](f: (A) => Seq[B]): Seq[B] = {
      implicitly[Monad[Seq]].bind(seq)(f)
    }

    def append[AA >: A : Monoid](a2: Seq[AA]): Seq[AA] = {
      implicitly[Monoid[Seq[AA]]].append(seq, a2)
    }
  }

  implicit def SeqMonoid[A : Monoid]: Monoid[Seq[A]] = new Monoid[Seq[A]] {
    override def zero: Seq[A] = Seq.empty[A]

    @tailrec
    override def append(a1: Seq[A], a2: Seq[A]): Seq[A] = a2 match {
      case Seq() => a1
      case head +: tail => append(a1 :+ head, tail)
    }
  }

  implicit val SeqMonad: Monad[Seq] = new Monad[Seq] {
    override def fmap[A, B](fa: Seq[A])(f: (A) => B): Seq[B] = {
      @tailrec
      def map(fa: Seq[A], fb: Seq[B]): Seq[B] = fa match {
        case Seq() => fb
        case head +: tail => map(tail, fb :+ f(head))
      }

      map(fa, Seq.empty[B])
    }

    override def point[A](a: A): Seq[A] = Seq(a)

    override def ap[A, B](fa: Seq[A])(f: Seq[(A) => B]): Seq[B] = {
      @tailrec
      def mapFunctions(f: Seq[(A) => B], fb: Seq[B]): Seq[B] = f match {
        case Seq() => fb
        case head +: tail => mapFunctions(tail, fb ++ fmap(fa)(head))
      }

      mapFunctions(f, Seq.empty[B])
    }

    override def bind[A, B](fa: Seq[A])(f: (A) => Seq[B]): Seq[B] = {
      @tailrec
      def flatMap(fa: Seq[A], fb: Seq[B]): Seq[B] = fa match {
        case Seq() => fb
        case head +: tail => flatMap(tail, fb ++ f(head))
      }

      flatMap(fa, Seq.empty[B])
    }
  }
}
