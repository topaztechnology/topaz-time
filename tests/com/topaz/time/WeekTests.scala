package com.topaz.time

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class WeekTests extends AnyFreeSpec with Matchers {
  "Ordinals should work as expected" in {
    val weeks = (0 to 1000).toSeq.map(Week(1990, 1) + _)
    weeks.zip(weeks.tail).foreach {
      case (w1, w2) =>
        w1.lastDay + 1 should equal(w2.firstDay)
        w1 + 1 should equal(w2)
        w1 should equal(w2 - 1)
    }
  }
}
