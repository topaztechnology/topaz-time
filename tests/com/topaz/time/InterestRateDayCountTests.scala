package com.topaz.time

import com.topaz.time.InterestRateDayCount._

import scala.language.reflectiveCalls
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class InterestRateDayCountTests extends AnyFreeSpec with Matchers
  with DateRangeSugar {

  "Single days should have zero time" in {

    for {
      day <- Seq(29 / Feb / 2016, 30 / Jun / 2016, 30 / Jul / 2016, 31 / Jul / 2016)
      dc <- InterestRateDayCount.values
    } {
      dc.timeBetween(day, day) shouldBe (0.0)
    }
  }

  "BusinessDaysDayCount" in {
    val dc = BusinessDaysDayCount(BusinessDayCalculation.weekdaysBusinessDayCalculation)
    dc.timeBetween(Friday / 10 / Jan / 2014, Friday / 10 / Jan / 2014) shouldBe 0
    dc.timeBetween(Friday / 10 / Jan / 2014, Saturday / 11 / Jan / 2014) shouldBe 1 / 252.0
    dc.timeBetween(Saturday / 11 / Jan / 2014, Saturday / 11 / Jan / 2014) shouldBe 0
    dc.timeBetween(Saturday / 11 / Jan / 2014, Sunday / 12 / Jan / 2014) shouldBe 0
    dc.timeBetween(Saturday / 11 / Jan / 2014, Monday / 13 / Jan / 2014) shouldBe 0
    dc.timeBetween(Sunday / 12 / Jan / 2014, Monday / 13 / Jan / 2014) shouldBe 0
    dc.timeBetween(Friday / 10 / Jan / 2014, Monday / 13 / Jan / 2014) shouldBe 1 / 252.0
  }

  "Act365" - {
    "non leap year" in {
      Act365.timeBetween(1 / Jan / 2015, 1 / Jan / 2016) shouldBe (1.0 +- 1e-9)
    }
    "leap year" in {
      Act365.timeBetween(1 / Jan / 2016, 1 / Jan / 2017) shouldBe (366.0 / 365 +- 1e-9)
    }
  }

  "Act360" - {
    "non leap year" in {
      Act360.timeBetween(1 / Jan / 2015, 1 / Jan / 2016) shouldBe (365.0 / 360 +- 1e-9)
    }
    "leap year" in {
      Act360.timeBetween(1 / Jan / 2016, 1 / Jan / 2017) shouldBe (366.0 / 360 +- 1e-9)
    }
  }

  "ActAct" - {
    "non leap years" in {
      ActAct.timeBetween(31 / Dec / 2017, 1 / Jan / 2018) shouldBe (1.0 / 365 +- 1e-9)
    }
    "last is leap year" in {
      ActAct.timeBetween(31 / Dec / 2015, 1 / Jan / 2016) shouldBe (1.0 / 365 +- 1e-9)
    }
    "first is leap year" in {
      ActAct.timeBetween(31 / Dec / 2016, 1 / Jan / 2017) shouldBe (1.0 / 366 +- 1e-9)
    }
    "Example from ISDA" in {
      // http://www.isda.org/c_and_a/pdf/ACT-ACT-ISDA-1999.pdf
      ActAct.timeBetween(1 / Nov / 2003, 1 / May / 2004) shouldBe (61.0 / 365 + 121.0 / 366 +- 1e-9)
    }
  }

  "30/360" - {
    "30/31 days get moved to 30" in {
      for {
        d1 <- Seq(30, 31)
        d2 <- Seq(30, 31)
      } {
        _30_360.timeBetween(d1 / Jan / 2016, d2 / Mar / 16) shouldBe (2.0 / 12 +- 1e-9)
      }
    }
    "d2 is left alone if d1 <= 29" in {
      for {
        d2 <- Seq(30, 31)
      } {
        _30_360.timeBetween(29 / Jan / 2016, d2 / Mar / 16) shouldBe (2.0 / 12  + (d2 - 29) / 360.0 +- 1e-9)
      }

    }
  }
}
