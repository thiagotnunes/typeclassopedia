package com.thiago.typeclassopedia

import com.thiago.typeclassopedia.Maybe._
import org.specs2.mutable.Specification

class MonadSpec extends Specification {
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
