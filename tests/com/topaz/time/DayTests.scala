package com.topaz.time

import com.topaz.utils.EitherTestPimps

import scala.language.reflectiveCalls
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class DayTests extends AnyFunSpec with Matchers with DateRangeTestsMixin with DateRangeSugar
  with EitherTestPimps
{
  describe("Day") {

    val samples = Vector(
      1 / Jan / 2010,
      25 / Dec / 2014,
      24 / Mar / 2015
    )

    it should behave like chronologicalDateRange(samples: _*)

    it should behave like sortingWorksCorrectly(15 / Jan / 10 to 10 / Apr / 10)

    it("should have working next/previous") {
      (1 / Jan / 2010).nextDay should equal(2 / Jan / 10)
      (1 / Jan / 2010).previousDay should equal(31 / Dec / 2009)
    }

    it("should return the next weekday") {
      val friday = 25 / Jul / 2014
      val saturday = friday.nextDay
      val sunday = saturday.nextDay
      val monday = sunday.nextDay
      val tuesday = monday.nextDay

      friday.nextWeekday should equal(monday)
      saturday.nextWeekday should equal(monday)
      sunday.nextWeekday should equal(monday)
      monday.nextWeekday should equal(tuesday)
    }

    it("should have to and until methods that operate in the same way as the scala collections") {
      (1 / Jan / 2010).until(3 / Jan / 10).size shouldBe 2
      (1 / Jan / 2010).until(3 / Jan / 10) shouldBe (1 / Jan / 2010).until(3 / Jan / 10, step = 1)
      (3 / Jan / 2010).until(1 / Jan / 10, step = -1) shouldBe Seq(
        3 / Jan / 2010, 2 / Jan / 2010
      )

      (1 / Jan / 2010).to(3 / Jan / 10).size shouldBe 3
      (1 / Jan / 2010).to(3 / Jan / 10) shouldBe (1 / Jan / 2010).to(3 / Jan / 10, step = 1)
      (3 / Jan / 2010).to(1 / Jan / 10, step = -1) shouldBe Seq(
        3 / Jan / 2010, 2 / Jan / 2010, 1 / Jan / 2010
      )
    }

    it("should be right about weekdays") {
      val friday = 25 / Jul / 2014
      val saturday = friday + 1
      val sunday = friday + 2
      assert(friday.isWeekday)
      assert(!friday.isWeekend)

      assert(saturday.isWeekend)
      assert(!saturday.isWeekday)

      assert(sunday.isWeekend)
      assert(!sunday.isWeekday)
    }

    it("should add weekdays") {
      val friday = 25 / Jul / 2014
      friday.addWeekdays(- 1) shouldEqual(24 / Jul / 2014)
      friday.addWeekdays(- 2) shouldEqual(23 / Jul / 2014)
      friday.addWeekdays(1) shouldEqual(28 / Jul / 2014)
      friday.addWeekdays(2) shouldEqual(29 / Jul / 2014)
    }

    it("should add weeks") {
      val friday = 25 / Jul / 2014
      friday.addWeeks(- 1) shouldEqual(Friday / 18 / Jul / 2014)
      friday.addWeeks(1) shouldEqual(Friday / 1 / Aug / 2014)
      friday.addWeeks(2) shouldEqual(Friday / 8 / Aug / 2014)
    }

    it("should add months") {
      15 / Mar / 2016 addMonths(2) should be (15 / May / 2016)
      15 / Mar / 2016 addMonths(-2) should be (15 / Jan / 2016)
      31 / Jan / 2016 addMonths(1) should be (29 / Feb / 2016)
      30 / Mar / 2016 addMonths(-1) should be (29 / Feb / 2016)
      28 / Feb / 2015 addMonths(1) should be (31 / Mar / 2015)
      30 / Sep / 2016 addMonths(-1) should be (31 / Aug / 2016)
    }

    it("Should format days according to http://en.wikipedia.org/wiki/ISO_8601") {
      (1 / Jan / 2000).toString should equal ("2000-01-01")
      (31 / Dec / 1999).toString should equal ("1999-12-31")
      (25 / Sep / 14).toString should equal ("2014-09-25")
    }

    it("should parse iso dates") {
      val days = Seq(1 / Jan / 2000, 31 / Dec / 1999, 25 / Sep / 14)
      days.foreach {
        day => Day.fromISO(day.toString) shouldEqual Some(day)
      }
    }

    it("should parse slash and hyphen separated dates") {
      DayParser.unapply("03/01/2012") shouldEqual Some(3 / Jan / 2012)
      DayParser.unapply("03-01-2012") shouldEqual Some(3 / Jan / 2012)
    }

    it("should parse slash DDD M MMM YY dates") {
      DayParser.unapply("Mon 5 May 08") shouldEqual Some(5 / May / 2008)
      DayParser.unapply("Mon 12 May 08") shouldEqual Some(12 / May / 2008)
    }

    it("should support excel day representation") {
      Day.fromExcel(42370.00) shouldBe Day(2016, 1, 1)
      Day.fromExcel(42429.00) shouldBe Day(2016, 2, 29)
      Day(2016, 2, 29).toExcel shouldBe 42429.00

      intercept[RuntimeException] {
        Day.fromExcel(0.0)
      }.getMessage shouldBe "0.0 is not a valid Excel day."

      intercept[RuntimeException] {
        Day.fromExcel(2019)
      }.getMessage shouldBe "2019.0 is not a valid Excel day."
    }

    it("should return day of week") {
      (20 / Mar / 2015).dayOfWeek should equal(Friday)
      (25 / Dec / 2014).dayOfWeek should equal(Thursday)
    }

    it("should move to preceding day of week") {
      (20 / Mar / 2015).moveToPreceding(Monday) should equal(16 / Mar / 2015)
      (20 / Mar / 2015).moveToPreceding(Saturday) should equal(14 / Mar / 2015)
      (Monday / 18 / Sep / 2017).moveToPreceding(Monday) should equal(Monday / 11 / Sep / 2017)
    }

    it("should move to next day of week") {
      (Monday / 18 / Sep / 2017).moveToNext(Monday) should equal(Monday / 25 / Sep / 2017)
      (Tuesday / 19 / Sep / 2017).moveToNext(Monday) should equal(Monday / 25 / Sep / 2017)
      (Sunday / 24 / Sep / 2017).moveToNext(Sunday) should equal(Sunday / 1 / Oct / 2017)
    }

    it("should round trip contract suffix") {
      1 / Jan / 2009 to 31 / Dec / 2011 foreach { day =>
        Day.fromContractIdentifierSuffix(day.toContractIdentifierSuffix).R shouldEqual day
      }
    }

    it("should convert from millis") {
      Day.fromMillisInLocalTime(1477934459000l) shouldEqual Day(2016, 10, 31)
    }
  }
}
