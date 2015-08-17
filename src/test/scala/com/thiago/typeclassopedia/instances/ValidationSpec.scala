package com.thiago.typeclassopedia.instances

import com.thiago.typeclassopedia.instances.Validation.{Failure, Success}
import com.thiago.typeclassopedia.instances.StringExtensions._
import com.thiago.typeclassopedia.instances.IntExtensions._
import org.specs2.mutable.Specification

class ValidationSpec extends Specification {
  "validation" >> {
    "maps over success" in {
      Success(1).map(_ + 1) ==== Success(2)
    }

    "maps over failure" in {
      Failure("error").map((e: Int) => e + 1) ==== Failure("error")
    }
  }

  "applicative" >> {
    "pure" >> {
      "returns success" in {
        Validation.pure(1) ==== Success(1)
      }
    }

    "ap" >> {
      "returns failure when first param is failure" in {
        Failure("error").ap(Success((e: Int) => e + 1)) ==== Failure("error")
      }

      "returns failure when function is failure" in {
        Success(1).ap(Failure("error")) ==== Failure("error")
      }

      "returns success when first param and function are successes" in {
        Success(1).ap(Success((e: Int) => e + 1)) ==== Success(2)
      }
    }
  }

  "semigroup" >> {
    "append" >> {
      "returns successes appended when both params are successes" in {
        Success[String, Int](1).append(Success(2)) ==== Success(3)
      }

      "returns failure when failure is appended to success" in {
        Failure("error").append(Success(1)) ==== Failure("error")
      }

      "returns failure when success is appended to failure" in {
        Success(1).append(Failure("error")) ==== Failure("error")
      }

      "returns failures appended when both params are failures" in {
        Failure[String, Int]("error1").append(Failure("error2")) ==== Failure("error1error2")
      }
    }
  }

  "monoid" >> {
    "zero" >> {
      "returns success for zero of param" in {
        Validation.zero[String, Int] ==== Success(0)
      }
    }
  }

  "monad" >> {
    "`return`" >> {
      "returns success" in {
        Validation.`return`(1) ==== Success(1)
      }
    }

    "flatMap" >> {
      "returns failure when flatMapping from failure" in {
        Failure("error").flatMap((e: Int) => Success(e + 1)) ==== Failure("error")
      }

      "returns failure when flatMapping from success, but function returns failure" in {
        Success(1).flatMap(_ => Failure("error")) ==== Failure("error")
      }

      "returns success when flatMapping from success and function returns success" in {
        Success(1).flatMap((e: Int) => Success(e + 1)) ==== Success(2)
      }
    }
  }
}
