package com.thiago.typeclassopedia

import org.specs2.mutable.Specification
import com.thiago.typeclassopedia.Functor._

class FunctorSpec extends Specification {
  "maps over sequence" in {
    SeqFunctor.fmap(Seq(1,2,3,4))(_ + 1) mustEqual Seq(2,3,4,5)
  }
}
