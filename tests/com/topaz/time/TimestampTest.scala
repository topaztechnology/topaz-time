package com.topaz.time

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TimestampTest extends AnyFunSuite with Matchers {

  test("to and from iso string") {
    new Timestamp(123457891011l).toISOString shouldEqual "1973-11-29T21:51:31.011Z"

    val clock = Clock.WallClock
    val ts = clock.now()
    Timestamp.parseISO(ts.toISOString) shouldEqual ts
  }
}
