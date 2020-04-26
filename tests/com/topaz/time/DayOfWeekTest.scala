package com.topaz.time

import com.topaz.time.DayOfWeek._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DayOfWeekTest extends AnyFunSuite with Matchers {

  test("names") {
    Monday.name shouldEqual "Monday"
    Friday.name shouldEqual "Friday"
  }

  test("from name") {
    DayOfWeek.withName("Monday") shouldEqual (Monday)
    DayOfWeek.withName("Friday") shouldEqual (Friday)
  }

}
