package com.thiago.typeclassopedia.instances

import org.specs2.mutable.Specification
import com.thiago.typeclassopedia.instances.StringExtensions._

class StringExtensionsSpec extends Specification {
  "semigroup" >> {
    "appends strings by concatenating them up" in {
      "hello".append(" world") ==== "hello world"
    }
  }

  "monoid" >> {
    "returns empty string for zero" in {
      StringExtensions.zero ==== ""
    }
  }
}
