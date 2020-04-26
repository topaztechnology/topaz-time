package com.topaz.time


import scala.language.reflectiveCalls
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class DecadeTests extends AnyFunSpec with Matchers with DateRangeTestsMixin with DateRangeSugar {
  describe("Decade") {
    val testSamples = Decade(2009, 1, 1) to Decade(2015, 12, 3)

    it should behave like chronologicalDateRange(testSamples: _*)
    it should behave like sortingWorksCorrectly(testSamples)

    it("should return first and last dates") {
      Decade(2015, 1, 1).firstDay should equal(1 / Jan / 2015)
      Decade(2015, 1, 1).lastDay should equal(10 / Jan / 2015)
      Decade(2015, 1, 2).firstDay should equal(11 / Jan / 2015)
      Decade(2015, 1, 2).lastDay should equal(20 / Jan / 2015)
      Decade(2015, 1, 3).firstDay should equal(21 / Jan / 2015)
      Decade(2015, 1, 3).lastDay should equal(31 / Jan / 2015)
      Decade(2015, 2, 1).firstDay should equal(1 / Feb / 2015)
      Decade(2015, 2, 1).lastDay should equal(10 / Feb / 2015)
      Decade(2015, 2, 2).firstDay should equal(11 / Feb / 2015)
      Decade(2015, 2, 2).lastDay should equal(20 / Feb / 2015)
      Decade(2015, 2, 3).firstDay should equal(21 / Feb / 2015)
      Decade(2015, 2, 3).lastDay should equal(28 / Feb / 2015)
    }

    it("should handle arithmetic operations") {

      Decade(2015, 1, 1) + 7 should equal(Decade(2015, 3, 2))
      Decade(2015, 1, 1) - 2 should equal(Decade(2014, 12, 2))
      Decade(2014, 11, 2) + 12 should equal(Decade(2015, 3, 2))
    }
  }
}
