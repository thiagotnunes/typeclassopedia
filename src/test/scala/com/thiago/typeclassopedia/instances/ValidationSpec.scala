package com.thiago.typeclassopedia.instances

import com.thiago.typeclassopedia.instances.Maybe.{Empty, Just}
import com.thiago.typeclassopedia.instances.Validation.{Failure, Success}
import org.specs2.mutable.Specification

class ValidationSpec extends Specification {
  "validation" >> {
    "maps over success" in {
      Success(1).map(_ + 1) mustEqual Success(2)
    }

    "maps over failure" in {
      Failure("error").map((e: Int) => e + 1) mustEqual Failure("error")
    }
  }

  "applicative" >> {
    "pure" >> {
      "returns success" in {
        Validation.pure(1) mustEqual Success(1)
      }
    }

    "<*>" >> {
      "returns failure when first param is failure" in {
        Failure("error").ap(Success((e: Int) => e + 1)) mustEqual Failure("error")
      }

      "returns failure when function is failure" in {
        Success(1).ap(Failure("error")) mustEqual Failure("error")
      }

      "returns success when first param and function are successes" in {
        Success(1).ap(Success((e: Int) => e + 1)) mustEqual Success(2)
      }
    }
  }

  "monad" >> {
    "`return`" >> {
      "returns success" in {
        Validation.`return`(1) mustEqual Success(1)
      }
    }

    "flatMap" >> {
      "returns failure when flatMapping from failure" in {
        Failure("error").flatMap((e: Int) => Success(e + 1)) mustEqual Failure("error")
      }

      "returns failure when flatMapping from success, but function returns failure" in {
        Success(1).flatMap(_ => Failure("error")) mustEqual Failure("error")
      }

      "returns success when flatMapping from success and function returns success" in {
        Success(1).flatMap((e: Int) => Success(e + 1)) mustEqual Success(2)
      }
    }
  }
}
