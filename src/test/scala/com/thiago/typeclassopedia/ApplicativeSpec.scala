package com.thiago.typeclassopedia

import com.thiago.typeclassopedia.Maybe._
import org.specs2.mutable.Specification

class ApplicativeSpec extends Specification {
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
      Empty().<*>(Just((e: Int) => e + 1)) mustEqual Empty()
    }

    "returns empty when function is not defined" in {
      Just(1).<*>(Empty[Int => Int]()) mustEqual Empty()
    }

    "returns just value when first param and function are defined" in {
      def numericValue(str: String): Maybe[Int] = {
        try {
          Just(str.toInt)
        } catch {
          case _: NumberFormatException => Empty()
        }
      }

      numericValue("1").<*>(numericValue("2").map((s: Int) => (f: Int) => f + s)) mustEqual Just(3)
    }
  }
}
