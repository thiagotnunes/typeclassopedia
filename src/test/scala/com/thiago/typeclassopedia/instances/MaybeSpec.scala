package com.thiago.typeclassopedia.instances

import com.thiago.typeclassopedia.instances.Maybe._
import com.thiago.typeclassopedia.instances.IntExtensions._
import org.specs2.mutable.Specification

class MaybeSpec extends Specification {
  "functor" >> {
    "maps over just" in {
      Just(1).map(_ + 1) ==== Just(2)
    }

    "maps over empty" in {
      Empty().map((e: Int) => e + 1) ==== Empty()
    }
  }
  
  "applicative" >> {
    "pure" >> {
      "returns maybe from non null value" in {
        Maybe.pure(1) ==== Just(1)
      }

      "returns maybe from null value" in {
        Maybe.pure(null) ==== Empty()
      }
    }

    "ap" >> {
      "returns empty when first param is not defined" in {
        Empty().ap(Just((e: Int) => e + 1)) ==== Empty()
      }

      "returns empty when function is not defined" in {
        Just(1).ap(Empty[Int => Int]()) ==== Empty()
      }

      "returns just value when first param and function are defined" in {
        Just(1).ap(Just((e: Int) => e + 1)) ==== Just(2)
      }
    }
  }

  "semigroup" >> {
    "append" >> {
      "returns just applied to append of inner values when both params are defined" in {
        Just(1).append(Just(2)) ==== Just(3)
      }

      "returns first param when first is defined but second is not" in {
        Just(1).append(Empty()) ==== Just(1)
      }

      "returns second param when second is defined but first is not" in {
        Empty[Int]().append(Just(1)) ==== Just(1)
      }

      "returns empty when both are not defined" in {
        Empty[Int]().append(Empty()) ==== Empty()
      }
    }
  }

  "monoid" >> {
    "zero" >> {
      "returns empty value" in {
        Maybe.zero[Int] ==== Empty()
      }
    }
  }

  "monad" >> {
    "`return`" >> {
      "returns maybe from non null value" in {
        Maybe.`return`(1) ==== Just(1)
      }

      "returns maybe from null value" in {
        Maybe.`return`(null) ==== Empty()
      }
    }

    "flatMap" >> {
      "returns empty when first param is not defined" in {
        Empty().flatMap((e: Int) => Just(e + 1)) ==== Empty()
      }

      "returns empty when function returns empty" in {
        Just(1).flatMap((e: Int) => Empty[Int]()) ==== Empty()
      }

      "returns just value when first param is defined and function returns defined value" in {
        Just(1).flatMap((e: Int) => Just(e + 1)) ==== Just(2)
      }
    }
  }
}
