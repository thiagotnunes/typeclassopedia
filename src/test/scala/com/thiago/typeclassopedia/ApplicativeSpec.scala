package com.thiago.typeclassopedia

import com.thiago.typeclassopedia.Applicative._
import org.specs2.mutable.Specification

class ApplicativeSpec extends Specification {
  "pure" >> {
    "returns option from non null value" in {
      OptionApplicative.pure(1) mustEqual Some(1)
    }

    "returns option from null value" in {
      OptionApplicative.pure(null) mustEqual None
    }
  }

  "<*>" >> {
    "returns none when first param is not defined" in {
      OptionApplicative.<*>(None)(Some((e: Int) => e + 1)) mustEqual None
    }

    "returns none when function is not defined" in {
      OptionApplicative.<*>(Some(1))(None) mustEqual None
    }

    "returns some value when first param and function are defined" in {
      def numericValue(str: String): Option[Int] = {
        try {
          Some(str.toInt)
        } catch {
          case _: NumberFormatException => None
        }
      }

      // FIXME: Remove map and use the functor one
      OptionApplicative.<*>(numericValue("1"))(numericValue("2").map((second: Int) => _ + second))
    }
  }
}
