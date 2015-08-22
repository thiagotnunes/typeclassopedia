package com.thiago.typeclassopedia.instances

import com.thiago.typeclassopedia.instances.IntExtensions._
import com.thiago.typeclassopedia.instances.SeqExtensions._
import org.specs2.mutable.Specification

class SeqExtensionsSpec extends Specification {

  "functor" >> {
    "maps over non empty sequence" in {
      Seq(1, 2).fmap(_ + 1) ==== Seq(2, 3)
    }

    "maps over empty sequence" in {
      Seq.empty[Int].fmap(_ + 1) ==== Seq.empty
    }
  }

  "applicative" >> {
    "pure" >> {
      "returns sequence with given element" in {
        SeqExtensions.pure(1) ==== Seq(1)
      }

      "ap" >> {
        "returns sequence with each element applied to the given functions" in {
          Seq(1, 2, 3).ap(Seq((e: Int) => e + 1, (e: Int) => e + 2, (e: Int) => e + 3)) ==== Seq(2, 3, 4, 3, 4, 5, 4, 5, 6)
        }

        "returns empty sequence when no functions are given" in {
          Seq(1, 2, 3).ap(Seq.empty) ==== Seq.empty
        }

        "returns empty sequence when ap'ing from empty sequence" in {
          Seq.empty.ap(Seq((e: Int) => e + 1, (e: Int) => e + 2, (e: Int) => e + 3)) ==== Seq.empty
        }
      }
    }
  }

  "semigroup" >> {
    "append" >> {
      "returns concatenated sequences from two non-empty sequences" in {
        Seq(1).append(Seq(2)) ==== Seq(1, 2)
      }

      "returns concatenated sequences from left sided non-empty sequence" in {
        Seq(1).append(Seq.empty) ==== Seq(1)
      }

      "returns concatenated sequences from right sided non-empty sequence" in {
        Seq.empty.append(Seq(1)) ==== Seq(1)
      }

      "returns empty sequence when appending two empty sequences" in {
        Seq.empty[Int].append(Seq.empty) ==== Seq.empty
      }
    }
  }

  "monoid" >> {
    "zero" >> {
      "returns empty sequence" in {
        SeqExtensions.zero[Int] ==== Seq.empty[Int]
      }
    }
  }

  "monad" >> {
    "`return`" >> {
      "returns sequence from given value" in {
        SeqExtensions.`return`(1) ==== Seq(1)
      }
    }

    "flatMap" >> {
      "returns flattened sequence" in {
        Seq(1, 2, 3).bind(Seq(_, 1)) ==== Seq(1, 1, 2, 1, 3, 1)
      }

      "returns empty sequence when bind'ing from empty sequence" in {
        Seq.empty[Int].bind(Seq(_, 1)) ==== Seq.empty
      }
    }
  }
}
