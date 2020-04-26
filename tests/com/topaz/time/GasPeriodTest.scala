package com.topaz.time

import com.topaz.utils.EitherTestPimps
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class GasPeriodTest extends AnyFreeSpec with Matchers
  with EitherTestPimps
  with DateRangeSugar {

  "Parse gas periods" in {
    // TODO daterange, quarter, season, etc
    val toTest = Seq(
      "2014-03-01-DA" -> GasDayAhead(Day(2014, 3, 1)),
      "2014-03-01-BOW" -> GasBOW(Day(2014, 3, 1)),
      "2014-02-26-SAT" -> GasSaturday(Day(2014, 2, 26)),
      "2014-02-26-SUN" -> GasSunday(Day(2014, 2, 26)),
      "2014-02-26-WE" -> GasWeekend(Day(2014, 2, 26)),
      "2014-02-26-WDNW" -> GasWorkingDaysNextWeek(Day(2014, 2, 26)),
      "2014-03-01-BOM" -> GasBOM(Day(2014, 3, 1)),
      "2014-03" -> GasMonth(Month(2014, 3)),
      "2014-GY" -> GasYear(2014),
      "2014-GSY" -> GasStorageYear(2014),
    )

    toTest.foreach {
      case (text, gp) =>
        gp.toPersistentString shouldEqual text
        GasPeriod.parse(text) shouldEqual gp
    }
  }

  "Day ahead" - {
    val day = Wednesday / 15 / May / 2019

    "should be tomorrow with no holidays" in {
      GasDayAhead(day).deliveryPeriod(EveryDayCalendar) shouldEqual day + 1
    }

    "should be next working day" in {
      GasDayAhead(day).deliveryPeriod(_.isTuesday) shouldEqual day.moveToNext(Tuesday)
    }
  }

  "Balance of week" - {
    val day = Wednesday / 15 / May / 2019

    "should be Thursday and Friday with no holidays" in {
      GasBOW(day).deliveryPeriod(EveryDayCalendar) shouldEqual DateRange(day + 1, day + 2)
    }

    "should be Thursday only when Friday is a holiday" in {
      GasBOW(day).deliveryPeriod(!_.isFriday) shouldEqual day + 1
    }

    "should be next week if fromDay is a Friday" in {
      val day = Friday / 17 / May / 2019
      GasBOW(day).deliveryPeriod(_ => true) shouldEqual DateRange(Tuesday / 21 / May / 2019, Friday / 24 / May / 2019)
    }

    "should be next week if fromDay is Thursday and Friday is a holiday" in {
      val day = Thursday / 16 / May / 2019
      GasBOW(day).deliveryPeriod(!_.isFriday) shouldEqual DateRange(Tuesday / 21 / May / 2019, Thursday / 23 / May / 2019)
    }

    "should be from next business day after Monday of next week if fromDay is a Friday" in {
      val day = Friday / 17 / May / 2019
      val expected = DateRange(Wednesday / 22 / May / 2019, Friday / 24 / May / 2019)
      GasBOW(day).deliveryPeriod(!_.isTuesday) shouldEqual expected
    }
  }

  "Saturday" in {
    val day = Wednesday / 15 / May / 2019

    GasSaturday(day).deliveryPeriod(EveryDayCalendar) shouldEqual Saturday / 18 / May / 2019
  }

  "Sunday" in {
    val day = Wednesday / 15 / May / 2019

    GasSunday(day).deliveryPeriod(EveryDayCalendar) shouldEqual Sunday / 19 / May / 2019
  }

  "Weekend" - {
    val day = Monday / 13 / May / 2019
    val week = day.containingWeek

    "should be Saturday and Sunday with no holidays" in {
      GasWeekend(day).deliveryPeriod(EveryDayCalendar) shouldEqual DateRange(week.saturday, week.sunday)
    }

    "should include adjacent holidays" in {
      GasWeekend(day).deliveryPeriod(!_.isFriday) shouldEqual
        DateRange(week.friday, week.sunday)
    }

    "should treat mid week holidays as weekends" in {
      GasWeekend(day).deliveryPeriod(d => !(d.isTuesday || d.isWednesday)) shouldEqual
        DateRange(week.tuesday, week.wednesday)
    }
  }

  "Working days next week" - {
    "should be all weekdays if no holidays" in {
      val day = Friday / 17 / May / 2019
      val nextWeek = day.containingWeek + 1
      GasWorkingDaysNextWeek(day).deliveryPeriod(_.isWeekday) shouldEqual DateRange(nextWeek.monday, nextWeek.friday)
    }

    "should be working days if holidays adjacent to weekends" in {
      val day = Thursday / 16 / May / 2019
      val nextWeek = day.containingWeek + 1
      GasWorkingDaysNextWeek(day).deliveryPeriod(d => d.isWeekday && !(d.isFriday || d.isMonday)) shouldEqual
        DateRange(nextWeek.tuesday, nextWeek.thursday)
    }

    "should be first working days in week from previous week if mid week holidays" in {
      val day = Friday / 17 / May / 2019
      val nextWeek = day.containingWeek + 1
      GasWorkingDaysNextWeek(day).deliveryPeriod(d => d.isWeekday && !(d.isTuesday || d.isWednesday)) shouldEqual
        nextWeek.monday
    }

    "should be last working days in week if mid week holidays" in {
      val day = Monday / 20 / May / 2019
      val thisWeek = day.containingWeek
      GasWorkingDaysNextWeek(day).deliveryPeriod(d => d.isWeekday && !(d.isTuesday || d.isWednesday)) shouldEqual
        DateRange(thisWeek.thursday, thisWeek.friday)
    }
  }

  "Balance of month" - {
    "should exclude next day ahead if sooner" in {
      val day = Thursday / 16 / May / 2019
      val month = day.containingMonth
      GasBOM(day).deliveryPeriod(EveryDayCalendar) shouldEqual month.remainder(day + 2).get
    }

    "should exclude weekend if sooner" in {
      val day = Friday / 17 / May / 2019
      val month = day.containingMonth
      GasBOM(day).deliveryPeriod(EveryDayCalendar) shouldEqual month.remainder(day.moveToNext(Monday)).get
    }

    "should exclude weekend with adjacent holidays if sooner" in {
      val day = Friday / 17 / May / 2019
      val month = day.containingMonth
      GasBOM(day).deliveryPeriod(!_.isMonday) shouldEqual month.remainder(day.moveToNext(Tuesday)).get
    }

    "should use next month if no working days after day ahead or weekend" in {
      val day = Friday / 28 / Jun / 2019
      GasBOM(day).deliveryPeriod(_.isWeekday) shouldEqual GasMonth(Jul / 2019).deliveryPeriod(_.isWeekday)
    }
  }
}
