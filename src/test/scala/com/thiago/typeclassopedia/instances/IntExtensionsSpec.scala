package com.thiago.typeclassopedia.instances

import org.specs2.mutable.Specification
import com.thiago.typeclassopedia.instances.IntExtensions._

class IntExtensionsSpec extends Specification {
  "semigroup" >> {
    "appends integers by summing them up" in {
      3.append(4) ==== 7
    }
  }

  "monoid" >> {
    "returns 0 for zero" in {
      IntExtensions.zero ==== 0
    }
  }
}
