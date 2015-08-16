package com.thiago.typeclassopedia

import org.specs2.mutable.Specification
import com.thiago.typeclassopedia.Maybe._

class FunctorSpec extends Specification {
  "maps over just" in {
    Just(1).fmap(_ + 1) mustEqual Just(2)
  }

  "maps over empty" in {
    Empty().fmap((e: Int) => e + 1) mustEqual Empty()
  }
}
