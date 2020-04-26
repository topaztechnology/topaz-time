package com.topaz.time

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers


class MarketTimeTest extends AnyFunSuite with Matchers with DateRangeSugar {

  test("to and from string") {
    Seq(
      MarketTime(10 / Jan / 2019, TimeOfDay.end),
      MarketTime(1 / Dec / 2019, TimeOfDay.start),
    ).foreach { mt =>
      MarketTime.unapply(mt.toString).get shouldEqual mt
    }

    MarketTime.unapply("2019-01-02 start").get shouldEqual MarketTime(2 / Jan / 2019, TimeOfDay.start)
    MarketTime.unapply("2019-01-02 end").get shouldEqual MarketTime(2 / Jan / 2019, TimeOfDay.end)
  }
}
