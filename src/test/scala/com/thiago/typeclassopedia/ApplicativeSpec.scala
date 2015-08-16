package com.thiago.typeclassopedia

import com.thiago.typeclassopedia.Maybe._
import org.specs2.mutable.Specification

class ApplicativeSpec extends Specification {
  "pure" >> {
    "returns option from non null value" in {
      Maybe.pure(1) mustEqual Just(1)
    }

    "returns option from null value" in {
      Maybe.pure(null) mustEqual Empty()
    }
  }

  "<*>" >> {
    "returns none when first param is not defined" in {
      Empty().<*>(Just((e: Int) => e + 1)) mustEqual Empty()
    }

    "returns none when function is not defined" in {
      Just(1).<*>(Empty[Int => Int]()) mustEqual Empty()
    }

    "returns some value when first param and function are defined" in {
      def numericValue(str: String): Maybe[Int] = {
        try {
          Just(str.toInt)
        } catch {
          case _: NumberFormatException => Empty()
        }
      }

      numericValue("1").<*>(numericValue("2").fmap((s: Int) => (f: Int) => f + s)) mustEqual Just(3)
    }
  }
}
