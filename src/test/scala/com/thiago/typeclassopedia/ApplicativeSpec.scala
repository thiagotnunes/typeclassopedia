package com.thiago.typeclassopedia

import com.thiago.typeclassopedia.Applicative._
import com.thiago.typeclassopedia.Functor._
import org.specs2.mutable.Specification

class ApplicativeSpec extends Specification {
  "pure" >> {
    "returns option from non null value" in {
      MaybeApplicative.pure(1) mustEqual Just(1)
    }

    "returns option from null value" in {
      MaybeApplicative.pure(null) mustEqual Empty()
    }
  }

  "<*>" >> {
    "returns none when first param is not defined" in {
      MaybeApplicative.<*>(Empty())(Just((e: Int) => e + 1)) mustEqual Empty()
    }

    "returns none when function is not defined" in {
      //FIXME: make it work without type hint for empty case
      MaybeApplicative.<*>(Just(1))(Empty[Int => Int]()) mustEqual Empty()
    }

    "returns some value when first param and function are defined" in {
      def numericValue(str: String): Maybe[Int] = {
        try {
          Just(str.toInt)
        } catch {
          case _: NumberFormatException => Empty()
        }
      }

      MaybeApplicative.<*>(numericValue("1"))(
        MaybeFunctor.fmap(numericValue("error"))(
          (second: Int) => (first: Int) => first + second
        )
      ) mustEqual Empty()
    }
  }
}
