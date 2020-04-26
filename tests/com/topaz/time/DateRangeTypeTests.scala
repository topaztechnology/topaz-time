package com.topaz.time

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DateRangeTypeTests extends AnyFreeSpec with Matchers{

  "to and from name" in {
    List(Year, Quarter, Decade, HalfMonth, Week, Month, Day).foreach {
      typ =>
        DateRangeType.fromName(typ.name) shouldEqual typ
    }
  }
}
