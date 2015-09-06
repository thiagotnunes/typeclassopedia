package com.thiago.typeclassopedia.definitions

trait Foldable[T[_]] {
  def foldMap[A, M](fa: T[A])(f: (A) => M)(implicit evidence: Monoid[M]): M
}
