package com.topaz.time

import com.topaz.utils.EitherTestPimps

import scala.language.reflectiveCalls
import scala.util.Random
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers


class DateRangeTests extends AnyFunSuite with Matchers with DateRangeSugar with EitherTestPimps {
  test("days") {
    SimpleDateRange(1 / Mar / 2014, 1 / Mar / 2014).days.toList shouldEqual 1 / Mar / 2014 :: Nil
    SimpleDateRange(1 / Mar / 2014, 2 / Mar / 2014).days.toList shouldEqual 1 / Mar / 2014 :: 2 / Mar / 2014 :: Nil
  }

  test("remainder") {
    Month(2010, 1).remainder(15 / Jan / 2011) shouldEqual None
    Month(2010, 1).remainder(15 / Jan / 2010) shouldEqual Some(BOM(15 / Jan / 2010))
    Month(2010, 1).remainder(15 / Jan / 2009) shouldEqual Some(Month(2010, 1))
  }

  test("from day to day") {
    Day(2012, 1, 1) to Day(2012, 1, 1) shouldEqual Vector(Day(2012, 1, 1))
    Day(2012, 1, 1) to Day(2012, 1, 3) shouldEqual Vector(Day(2012, 1, 1),Day(2012, 1, 2),Day(2012, 1, 3))
    intercept[IllegalArgumentException] {
      Day(2012, 1, 2) to Day(2012, 1, 1)
    }
  }

  test("range size") {
    (Day(2012, 1, 1) to Day(2012, 1, 1)).size shouldEqual 1
    (Day(2012, 1, 1) to Day(2012, 1, 3)).size shouldEqual 3
  }

  test("parse month") {
    Month.unapply("2010-1") shouldEqual Some(Month(2010, 1))
    Month.unapply("2010-01") shouldEqual Some(Month(2010, 1))
    Month.unapply("2010-12") shouldEqual Some(Month(2010, 12))
    Month.unapply("10-12") shouldEqual None
    Month.unapply("20102-12") shouldEqual None
    Month.unapply("2010-121") shouldEqual None
    Month.unapply("3500-12") shouldEqual None
    Month.unapply("2010-13") shouldEqual None
  }

  test("parse using DateRange.unapply") {
    DateRange.unapply("cal-19") shouldEqual Some(Year(2019))
    DateRange.unapply("cal-2019") shouldEqual Some(Year(2019))
    DateRange.unapply("2019") shouldEqual Some(Year(2019))
    DateRange.unapply("2019-11") shouldEqual Some(Month(2019, 11))
    DateRange.unapply("Nov-2019") shouldEqual Some(Month(2019, 11))
    DateRange.unapply("Nov-19") shouldEqual Some(Month(2019, 11))
    DateRange.unapply("2019-11-01") shouldEqual Some(Day(2019, 11, 1))
    DateRange.unapply("2019-q1") shouldEqual Some(Quarter(2019, 1))
    DateRange.unapply("q1-19") shouldEqual Some(Quarter(2019, 1))
    DateRange.unapply("q1-2019") shouldEqual Some(Quarter(2019, 1))
    DateRange.unapply("2019-F") shouldEqual Some(HalfYear(2019, 1))
    DateRange.unapply("2019-1H") shouldEqual Some(HalfYear(2019, 1))
    DateRange.unapply("2019-2H") shouldEqual Some(HalfYear(2019, 2))
    DateRange.unapply("19-2H") shouldEqual None
    DateRange.unapply("2019-gassum") shouldEqual Some(GasSeason(2019, isSummer = true))
    DateRange.unapply("2019-gassummer") shouldEqual Some(GasSeason(2019, isSummer = true))
    DateRange.unapply("2019-GasSummer") shouldEqual Some(GasSeason(2019, isSummer = true))
    DateRange.unapply("2019-gaswin") shouldEqual Some(GasSeason(2019, isSummer = false))
    DateRange.unapply("2019-gaswinter") shouldEqual Some(GasSeason(2019, isSummer = false))
    DateRange.unapply("2019-GasWinter") shouldEqual Some(GasSeason(2019, isSummer = false))
    DateRange.unapply("2019-GY") shouldEqual Some(GasYear(2019))
    DateRange.unapply("2019-GSY") shouldEqual Some(GasStorageYear(2019))
    DateRange.unapply("2019-02:3") shouldEqual Some(Decade(2019, 2, 3))
  }

  test("more parsing") {
    val toTest = Seq(
      "3-31/7/2014" -> SimpleDateRange(3 / Jul / 2014, 31 / Jul / 2014),
      "3-8/7/2014" -> SimpleDateRange(3 / Jul / 2014, 8 / Jul / 2014)
    )
    toTest.foreach {
      case (str, expected) =>
        DateRange.unapply(str) shouldEqual Some(expected)
    }
  }
  
  test("normaliseDateRangeIfPossible") {
    def calendar(day: Day) = {
      !(day == 1 / Jul / 2014 || day == 31 / Jul / 2014)
    }

    for(dr <- List(
      SimpleDateRange(1 / Jul / 2014, 31 / Jul / 2014),
      SimpleDateRange(1 / Jul / 2014, 30 / Jul / 2014),
      SimpleDateRange(2 / Jul / 2014, 30 / Jul / 2014),
      SimpleDateRange(2 / Jul / 2014, 31 / Jul / 2014))) {
      dr.normaliseToMonth(calendar) shouldEqual Some(Month(2014, 7))
    }
    SimpleDateRange(3 / Jul / 2014, 30 / Jul / 2014).normaliseToMonth(calendar) shouldEqual None
    SimpleDateRange(3 / Jul / 2014, 31 / Jul / 2014).normalise shouldEqual Some(BOM(3 / Jul / 2014))
  }

  test("persistent string representations shouldn't change without us knowing about it") {
    SimpleDateRange(2 / Jul / 2014, 30 / Jul / 2014).toPersistentString shouldEqual "2014-07-02->2014-07-30"
    Month(2010, 1).toPersistentString shouldEqual "2010-01"
    Day(2012, 1, 2).toPersistentString shouldEqual "2012-01-02"
    HalfMonth(2010, 2, 1).toPersistentString shouldEqual "2010-02F"
    HalfMonth(2010, 2, 2).toPersistentString shouldEqual "2010-02B"
    Decade(2010, 2, 3).toPersistentString shouldEqual "2010-02:3"
    Year(2010).toPersistentString shouldEqual "2010"
    Quarter(2015, 1).toPersistentString shouldEqual "2015-q1"
    Quarter(2015, 4).toPersistentString shouldEqual "2015-q4"
    GasSeason(2015, isSummer = true).toPersistentString shouldEqual "2015-GasSummer"
    GasSeason(2015, isSummer = false).toPersistentString shouldEqual "2015-GasWinter"
  }

  test("two digit strings") {
    Day(2012, 1, 2).containingYear.toTwoDigitString shouldEqual "12"
    Day(2012, 1, 2).containingMonth.toTwoDigitString shouldEqual "01"
    Day(2012, 1, 2).toTwoDigitString shouldEqual "02"
  }

  test("should round trip through persistent string and back again") {
    List(
      SimpleDateRange(2 / Jul / 2014, 30 / Jul / 2014),
      Month(2010, 1),
      Week(2014, 10),
      Week(2014, 1),
      Day(2012, 1, 2),
      HalfMonth(2010, 2, 1),
      HalfMonth(2010, 2, 2),
      Decade(2010, 2, 3),
      Quarter(2015, 1),
      Quarter(2015, 4),
      GasSeason(2015, isSummer = true),
      GasSeason(2015, isSummer = false),
      Year(2016)
    ).foreach {
      dr => 
        DateRange.unapply(dr.toPersistentString) shouldEqual Some(dr)
    }
  }

  test("year sorting") {
    List(Year(2016), Year(2014), Year(2015)).sorted shouldEqual List(Year(2014), Year(2015), Year(2016))
  }

  test("quarter") {
    Quarter.containing(Day(2015, 1, 1)) shouldEqual Quarter(2015, 1)
    Quarter.containing(Day(2015, 6, 1)) shouldEqual Quarter(2015, 2)
    Quarter.containing(Day(2015, 7, 1)) shouldEqual Quarter(2015, 3)
    Quarter.containing(Day(2015, 12, 31)) shouldEqual Quarter(2015, 4)

    Quarter(2014, 1).firstDay shouldEqual 1 / Jan / 2014
    Quarter(2014, 1).lastDay shouldEqual 31 / Mar / 2014
    Quarter(2014, 2).firstDay shouldEqual 1 / Apr / 2014
    Quarter(2014, 2).lastDay shouldEqual 30 / Jun / 2014
    Quarter(2014, 3).firstDay shouldEqual 1 / Jul / 2014
    Quarter(2014, 3).lastDay shouldEqual 30 / Sep / 2014
    Quarter(2014, 4).firstDay shouldEqual 1 / Oct / 2014
    Quarter(2014, 4).lastDay shouldEqual 31 / Dec / 2014

    (Quarter(2014, 4) + 1) shouldEqual Quarter(2015, 1)
    (Quarter(2014, 1) - 1) shouldEqual Quarter(2013, 4)
  }

  test("intersection") {
    val rand = new Random(1234)
    val days = SimpleDateRange(1 / Jan / 2014, 31 / Dec / 2014).days.toIndexedSeq
    (0 to 100).foreach {
      _ => {
        val d1 = days(rand.nextInt(364))
        val d2 = days(rand.nextInt(364))
        val d3 = days(rand.nextInt(364))
        val d4 = days(rand.nextInt(364))
        if (d1 <= d2 && d3 <= d4) {
          val dr1 = SimpleDateRange(d1, d2)
          val dr2 = SimpleDateRange(d3, d4)

          val intersect = dr1.days.intersect(dr2.days) match {
            case IndexedSeq() => None
            case intersection => Some(DateRange(intersection.head, intersection.last))
          }
          dr1.intersect(dr2) shouldEqual intersect
        }
      }
    }
  }

  test("intersection returns correct types") {
    Month(2015, 6).intersect(Month(2015, 6)) shouldEqual Some(Month(2015, 6))
    Day(2015, 6, 1).intersect(Day(2015, 6, 1)) shouldEqual Some(Day(2015, 6, 1))
    Year(2015).intersect(Year(2015)) shouldEqual Some(Year(2015))
  }

  test("split by periodicity") {
    Quarter(2014, 1).splitByPeriodicity(Month).R shouldEqual Seq(Month(2014, 1), Month(2014, 2), Month(2014, 3))
    Quarter(2014, 4).splitByPeriodicity(Month).R shouldEqual Seq(Month(2014, 10),Month(2014, 11),Month(2014, 12))

    Month(2014, 10).splitByPeriodicity(Month).R shouldEqual Seq(Month(2014, 10))

    Month(2014, 10).splitByPeriodicity(Day).R shouldEqual Month(2014, 10).days

    DateRange(Day(2014, 6, 1), Day(2014, 7, 31)).splitByPeriodicity(Month).R shouldEqual
      Seq(Month(2014, 6), Month(2014, 7))

    DateRange(Day(2014, 6, 13), Day(2014, 7, 31)).splitByPeriodicity(Month).R shouldEqual
      Seq(DateRange(Day(2014, 6, 13), Day(2014, 6, 30)), Month(2014, 7))

    DateRange(Day(2014, 6, 13), Day(2014, 7, 31)).splitByPeriodicity(Month).R shouldEqual
      Seq(DateRange(Day(2014, 6, 13), Day(2014, 6, 30)), DateRange(Day(2014, 7, 1), Day(2014, 7, 31)))

  }

  test("extended season") {
    GasSeason(2016, isSummer = true).months shouldEqual (Apr / 2016 to Sep / 2016)
    GasSeason(2016, isSummer = false).months shouldEqual (Oct / 2016 to Mar / 2017)
  }

  test("week") {
    Week.firstDayOfYear(2016).dayOfWeek shouldEqual Monday
    Week(2016, 1).firstDay shouldEqual Week.firstDayOfYear(2016)

    Week(2015, 23).wednesday shouldEqual Day(2015, 6, 3)
    Week(2015, 25).tuesday shouldEqual Day(2015, 6, 16)
    Week(2015, 27).sunday shouldEqual Day(2015, 7, 5)

    (1950 to 2150).foreach {
      year =>
        Week.containing(Day(year, 1, 4)) shouldEqual Week(year, 1)
        Week.containing(Day(year, 1, 4).thisOrPreceding(Monday)) shouldEqual Week(year, 1)
        Week.containing(Day(year, 1, 4)).firstDay shouldEqual Day(year, 1, 4).thisOrPreceding(Monday)
    }
  }

  test("containing period") {
    (1 / Jan / 2020).singleContainingMonth shouldEqual Some(Jan / 2020)
    (1 / Jan / 2020).singleContainingQuarter shouldEqual Some(Quarter(2020, 1))
    (1 / Jan / 2020).singleContainingYear shouldEqual Some(Year(2020))

    Quarter(2020, 1).singleContainingMonth shouldEqual None
    Year(2020).singleContainingQuarter shouldEqual None
    DateRange(1 / Dec / 2020, 1 / Jan / 2021).singleContainingYear shouldEqual None
  }
  
  test("half year") {
    Seq(
      "1h-16" -> Some(HalfYear(2016, 1)),
      "2h-17" -> Some(HalfYear(2017, 2)),
      "3h-16" -> None,
      "2016-f" -> Some(HalfYear(2016, 1)),
      "2016-B" -> Some(HalfYear(2016, 2)),
      "2016-1" -> None
    ).foreach {
      case (str, hy) =>
        HalfYear.unapply(str) shouldEqual hy
    }
    
    HalfYear(2016, 1).firstDay shouldEqual Day(2016, 1, 1)
    HalfYear(2016, 1).lastDay shouldEqual Day(2016, 6, 30)
    HalfYear(2016, 2).firstDay shouldEqual Day(2016, 7, 1)
    HalfYear(2016, 2).lastDay shouldEqual Day(2016, 12, 31)
  }
}
