package com.topaz.time


import scala.language.reflectiveCalls
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class HalfMonthTests extends AnyFunSpec with Matchers with DateRangeTestsMixin with DateRangeSugar {
  describe("HalfMonth") {
    val testSamples = HalfMonth(2009, 1, 1) to HalfMonth(2015, 12, 2)

    it should behave like chronologicalDateRange(testSamples: _*)
    it should behave like sortingWorksCorrectly(testSamples)

    it("should return first and last dates") {
      HalfMonth(2015, 1, 1).firstDay should equal(1 / Jan / 2015)
      HalfMonth(2015, 1, 1).lastDay should equal(15 / Jan / 2015)
      HalfMonth(2015, 1, 2).firstDay should equal(16 / Jan / 2015)
      HalfMonth(2015, 1, 2).lastDay should equal(31 / Jan / 2015)
      HalfMonth(2015, 2, 1).firstDay should equal(1 / Feb / 2015)
      HalfMonth(2015, 2, 1).lastDay should equal(15 / Feb / 2015)
      HalfMonth(2015, 2, 2).firstDay should equal(16 / Feb / 2015)
      HalfMonth(2015, 2, 2).lastDay should equal(28 / Feb / 2015)
    }

    it("should handle arithmetic operations") {
      HalfMonth(2015, 1, 1) + 7 should equal(HalfMonth(2015, 4, 2))
      HalfMonth(2015, 1, 1) - 3 should equal(HalfMonth(2014, 11, 2))
      HalfMonth(2014, 11, 1) + 7 should equal(HalfMonth(2015, 2, 2))
    }
  }
}
