package com.thiago.typeclassopedia.instances

import com.thiago.typeclassopedia.instances.Validation.{Failure, Success}
import org.specs2.mutable.Specification

class ValidationSpec extends Specification {
  "validation" >> {
    "maps over success" in {
      Success[String, Int](1).map[Int]((e: Int) => e + 1) mustEqual Success(2)
    }

    "maps over failure" in {
      Failure[String, Int]("error").map[Int]((e: Int) => e + 1) mustEqual Failure("error")
    }
  }
}
