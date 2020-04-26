package com.topaz.time

import com.topaz.utils.EitherTestPimps
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class MonthTests extends AnyFunSpec with Matchers with DateRangeTestsMixin
  with DateRangeSugar
  with EitherTestPimps
{
  describe("Month") {
    val testSamples = Jan / 10 to Mar / 15

    it should behave like chronologicalDateRange(testSamples: _*)

    it should behave like sortingWorksCorrectly(Jan / 10 to Jun / 11)

    it("should get previous/next") {
      (Jan / 14).previous should equal(Dec / 13)
      (Jan / 14).next should equal(Feb / 14)
      (Dec / 13).next should equal(Jan / 14)
      (Dec / 13).previous should equal(Nov / 13)
    }

    it("should return a seq of months") {
      (Jan / 14) to (Mar / 14) shouldEqual Vector(Jan / 14, Feb / 14, Mar / 14)
    }
    
    it("should parse a range") {
      val toTest = Seq(
        "2014-01->2014-08" -> Some(Month(2014, 1) -> Month(2014, 8)),
        "2014-01->2015-01" -> Some(Month(2014, 1) -> Month(2015, 1)),
        "2014-12->2015-01" -> Some(Month(2014, 12) -> Month(2015, 1)),
        "2015-12->2015-01" -> None
      )
      toTest.foreach {
        case (str, months) =>
          Month.parseRange(str) shouldEqual months
      }
    }

    it("should parse MMM-YY months") {
      Seq(("Dec-14", Dec / 14), ("Jan-11", Jan / 11)).foreach {
        case (str, month) => Month.unapply(str) shouldEqual Some(month)
      }
    }

    it("should not throw on invalid months and years") {
      Seq("2020-0", "11-10").foreach { str => Month.unapply(str) shouldEqual None }
    }

    it("Should format months according to http://en.wikipedia.org/wiki/ISO_8601") {
      (Jan / 14).toString should equal ("2014-01")
      (Nov / 2001).toString should equal ("2001-11")
    }

    it("should have the right month names") {
      val names =
        Vector(
          1 -> "January",
          2 -> "February",
          3 -> "March",
          4 -> "April",
          5 -> "May",
          6 -> "June",
          7 -> "July",
          8 -> "August",
          9 -> "September",
          10 -> "October",
          11 -> "November",
          12 -> "December")
      names.foreach {
        case (i, name) =>
          Month.months(i - 1) shouldEqual name
      }
      
      names.foreach { case (index, name) =>
        Month(2020, index).monthName shouldEqual name
      }
    }
  }

  it("should produce correct results for first and last") {
    Month(2015, 6).first(Monday) shouldEqual Day(2015, 6, 1)
    Month(2015, 7).first(Monday) shouldEqual Day(2015, 7, 6)
    Month(2015, 7).first(Tuesday) shouldEqual Day(2015, 7, 7)
    Month(2015, 7).first(Wednesday) shouldEqual Day(2015, 7, 1)
    Month(2015, 7).first(Thursday) shouldEqual Day(2015, 7, 2)
    Month(2015, 7).first(Friday) shouldEqual Day(2015, 7, 3)
    Month(2015, 7).first(Saturday) shouldEqual Day(2015, 7, 4)
    Month(2015, 7).first(Sunday) shouldEqual Day(2015, 7, 5)

    Month(2015, 7).last(Tuesday) shouldEqual Day(2015, 7, 28)
    Month(2015, 7).last(Wednesday) shouldEqual Day(2015, 7, 29)
    Month(2015, 7).last(Saturday) shouldEqual Day(2015, 7, 25)
  }

  it("should do month of year correctly") {
    Month(2015, 1).monthOfYear shouldEqual Jan
    Month(2015, 5).monthOfYear shouldEqual May
    Month(2015, 7).monthOfYear shouldEqual Jul
    Month(2015, 12).monthOfYear shouldEqual Dec

    Month(2015, 12).thisOrNext(Jan) shouldEqual Month(2016, 1)
    Month(2015, 11).thisOrNext(Jan) shouldEqual Month(2016, 1)
    Month(2015, 6).thisOrNext(Jan) shouldEqual Month(2016, 1)
    Month(2015, 1).thisOrNext(Jan) shouldEqual Month(2015, 1)
    Month(2015, 1).thisOrNext(Jun) shouldEqual Month(2015, 6)
    Month(2015, 10).thisOrNext(Jun) shouldEqual Month(2016, 6)
  }

  it("should round trip contract suffix") {
    Month(2005, 1) to Month(2030, 12) foreach { month =>
      Month.fromContractIdentifierSuffix(month.toContractIdentifierSuffix).R
    }
  }

  describe("month of year next") {
    Jan.next shouldEqual Feb
    Dec.next shouldEqual Jan
  }
}
