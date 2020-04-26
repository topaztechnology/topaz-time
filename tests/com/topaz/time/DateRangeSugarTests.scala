package com.topaz.time


import scala.language.reflectiveCalls
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class DateRangeSugarTests extends AnyFunSpec with Matchers with DateRangeSugar {
  describe("DateRangeSugar") {
    it("Should construct days") {
      (1 / Jan / 2014) should equal(Day(2014, 1, 1))
    }
    it("Should construct months") {
      Jan / 2014 should equal(Month(2014, 1))
    }
  }
}
