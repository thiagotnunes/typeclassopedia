package com.thiago.typeclassopedia.instances

import com.thiago.typeclassopedia.instances.Maybe._
import org.specs2.mutable.Specification

class MaybeSpec extends Specification {
  "functor" >> {
    "maps over just" in {
      Just(1).map(_ + 1) mustEqual Just(2)
    }

    "maps over empty" in {
      Empty().map((e: Int) => e + 1) mustEqual Empty()
    }
  }
  
  "applicative" >> {
    "pure" >> {
      "returns maybe from non null value" in {
        Maybe.pure(1) mustEqual Just(1)
      }

      "returns maybe from null value" in {
        Maybe.pure(null) mustEqual Empty()
      }
    }

    "<*>" >> {
      "returns empty when first param is not defined" in {
        Empty().ap(Just((e: Int) => e + 1)) mustEqual Empty()
      }

      "returns empty when function is not defined" in {
        Just(1).ap(Empty[Int => Int]()) mustEqual Empty()
      }

      "returns just value when first param and function are defined" in {
        Just(1).ap(Just((e: Int) => e + 1)) mustEqual Just(2)
      }
    }
  }

  "monad" >> {
    "`return`" >> {
      "returns maybe from non null value" in {
        Maybe.`return`(1) mustEqual Just(1)
      }

      "returns maybe from null value" in {
        Maybe.`return`(null) mustEqual Empty()
      }
    }

    "flatMap" >> {
      "returns empty when first param is not defined" in {
        Empty().flatMap((e: Int) => Just(e + 1)) mustEqual Empty()
      }

      "returns empty when function returns empty" in {
        Just(1).flatMap((e: Int) => Empty[Int]()) mustEqual Empty()
      }

      "returns just value when first param is defined and function returns defined value" in {
        Just(1).flatMap((e: Int) => Just(e + 1)) mustEqual Just(2)
      }
    }
  }
}
