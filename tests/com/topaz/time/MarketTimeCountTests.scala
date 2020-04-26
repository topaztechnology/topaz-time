package com.topaz.time

import com.topaz.time.MarketTimeCount._
import com.topaz.utils.{RandomGeneratorPimps, RandomTestSuite}

import scala.language.{postfixOps, reflectiveCalls}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class MarketTimeCountTests extends AnyFreeSpec with Matchers
  with DateRangeSugar with RandomTestSuite with RandomGeneratorPimps {

  "Act365" - {
    "start of day to end of day on same day" in {
      val day = Monday / 18 / Sep / 2017
      Act365.timeBetween(day.endOfDay, day.endOfDay) shouldEqual 0.0
      Act365.timeBetween(day.startOfDay, day.endOfDay) shouldEqual 1.0 / 365.0
      Act365.timeBetween(day.startOfDay, day.startOfDay) shouldEqual 0.0
      Act365.timeBetween(day.endOfDay, day.startOfDay) shouldEqual -1.0 / 365.0
    }
    
    "eod to sod next day" in {
      val t1 = 18 / Sep / 2017 endOfDay
      val t2 = 19 / Sep / 2017 startOfDay; // ; needed for some reason
      Act365.timeBetween(t1, t2) shouldEqual 0.0
    }
    
    "random" in {
      withRandomGenerator(numTests = 100) {
        rng =>
          def calc(t1: MarketTime, t2: MarketTime) = {
            val toAdd = if (t2.timeOfDay > t1.timeOfDay)
              1
            else if (t2.timeOfDay < t1.timeOfDay)
              -1
            else
              0
            (t2.day.ordinal - t1.day.ordinal + toAdd) / 365.0
          }

          val t1 = MarketTime(Day(2017, 3, 1) + rng.nextInt(50), rng.nextThing(Seq(TimeOfDay.start, TimeOfDay.end)))
          val t2 = MarketTime(t1.day + rng.nextInt(500), rng.nextThing(Seq(TimeOfDay.start, TimeOfDay.end)))

          calc(t1, t2) * 365.0 shouldEqual (Act365.timeBetween(t1, t2) *  365.0)
      }
    }
  }
  
  "CalendarDayCount" - {
    "same as Weekdays252 when we're using a weekday calendar" in {
      val calDC = BusinessDaysMarketTime(BusinessDayCalculation.weekdaysBusinessDayCalculation)
      withRandomGenerator(numTests = 100) {
        rng =>
          val t1 = MarketTime(Day(2017, 3, 1) + rng.nextInt(50), rng.nextThing(Seq(TimeOfDay.start, TimeOfDay.end)))
          val t2 = MarketTime(t1.day + rng.nextInt(500), rng.nextThing(Seq(TimeOfDay.start, TimeOfDay.end)))

          calDC.timeBetween(t1, t2) shouldEqual Weekdays252.timeBetween(t1, t2)
      }
    }

    // Slow but correct implementation to test against
    def timeBetween(isBusinessDay: Day => Boolean, t1: MarketTime, t2: MarketTime): Double = {
      if (t1 > t2) {
        -timeBetween(isBusinessDay, t2, t1)
      } else {
        var t = t1
        var businessDays = 0
        while (t != t2) {
          t = t.next()
          if (t.timeOfDay == TimeOfDay.end && isBusinessDay(t.day)) {
            businessDays += 1
          }
        }
        businessDays / 252.0
      }
    }

    "random with weekdays calendar" in {
      val calDC = BusinessDaysMarketTime(BusinessDayCalculation.weekdaysBusinessDayCalculation)

      withRandomGenerator(numTests = 100) {
        rng =>
          val t1 = MarketTime(Day(2017, 3, 1) + rng.nextInt(50), rng.nextThing(Seq(TimeOfDay.start, TimeOfDay.end)))
          val t2 = MarketTime(t1.day + rng.nextInt(500), rng.nextThing(Seq(TimeOfDay.start, TimeOfDay.end)))

          timeBetween(_.isWeekday, t1, t2) shouldEqual calDC.timeBetween(t1, t2)
      }
    }

    "with random calendar" in {

      withRandomGenerator(numTests = 100) {
        rng =>
          val t1 = MarketTime(Day(2017, 3, 1) + rng.nextInt(50), rng.nextThing(Seq(TimeOfDay.start, TimeOfDay.end)))
          val t2 = MarketTime(t1.day + rng.nextInt(500), rng.nextThing(Seq(TimeOfDay.start, TimeOfDay.end)))

          val holidays = (t1.day to t2.day).filter(_ => rng.nextInt(100) < 10).toSet

          def isBusinessDay(day: Day) = !holidays.contains(day)

          val calDC = BusinessDaysMarketTime(new BusinessDayCalculation {

            def name: String = "test"

            def businessDaysBetween(d1: Day, d2: Day): Int = {
              require(d2 >= d1, s"d1 ($d1) must be after d2 ($d2)")
              var c = 0
              var d = d1
              while(d != d2) {
                if (isBusinessDay(d))
                  c += 1
                d = d.nextDay
              }
              c
            }

            def isBusinessDay(day: Day): Boolean = !holidays.contains(day)

            def nextBusinessDay(day: Day): Day = {
              var d = day
              while (!isBusinessDay(d)) {
                d = d.nextDay
              }
              d
            }

            def previousBusinessDay(day: Day): Day = {
              var d = day
              while (!isBusinessDay(d)) {
                d = d.previousDay
              }
              d
            }
          })
          val expected = timeBetween(isBusinessDay, t1, t2)
          calDC.timeBetween(t1, t2) shouldEqual expected
          calDC.calculationDescription(t1, t2) should startWith ("BusinessDays(test, ")
      }
    }
  }

  "Weekdays" in {
    Weekdays.weekDaysBetween(Friday / 10 / Jan / 2014, Friday / 10 / Jan / 2014) shouldBe 0
    Weekdays.weekDaysBetween(Friday / 10 / Jan / 2014, Saturday / 11 / Jan / 2014) shouldBe 1
    Weekdays.weekDaysBetween(Saturday / 11 / Jan / 2014, Saturday / 11 / Jan / 2014) shouldBe 0
    Weekdays.weekDaysBetween(Saturday / 11 / Jan / 2014, Sunday / 12 / Jan / 2014) shouldBe 0
    Weekdays.weekDaysBetween(Saturday / 11 / Jan / 2014, Monday / 13 / Jan / 2014) shouldBe 0
    Weekdays.weekDaysBetween(Sunday / 12 / Jan / 2014, Monday / 13 / Jan / 2014) shouldBe 0
    Weekdays.weekDaysBetween(Friday / 10 / Jan / 2014, Monday / 13 / Jan / 2014) shouldBe 1

    var c = 0
    (Wednesday / 8 / Jan / 2014 to Sunday / 12 / Jan / 2014).foreach {
      d =>
        c += Weekdays.weekDaysBetween(d, d + 1)
    }
    c shouldEqual Weekdays.weekDaysBetween(Wednesday / 8 / Jan / 2014, Monday / 13 / Jan / 2014)
  }

  "Weekdays261" - {
    "timeBetween for one year is 1.0" in {
      val d1 = 18 / Sep / 2013
      val d2 = 18 / Sep / 2014
      Weekdays261.timeBetween(d1.endOfDay, d2.endOfDay) shouldEqual 1.0
    }
    
    "start of day to end of day on same day" in {
      val day = Monday / 18 / Sep / 2017
      Weekdays261.timeBetween(day.endOfDay, day.endOfDay) shouldEqual 0.0
      Weekdays261.timeBetween(day.startOfDay, day.endOfDay) shouldEqual 1.0 / 261.0
      Weekdays261.timeBetween(day.startOfDay, day.startOfDay) shouldEqual 0.0
      Weekdays261.timeBetween(day.endOfDay, day.startOfDay) shouldEqual -1.0 / 261.0
    }
    
    "sod to eod over weekend" in {
      def days(t1: MarketTime, t2: MarketTime) = Weekdays261.timeBetween(t1, t2) * 261.0

      days(Friday / 22 / Sep / 2017 startOfDay, Friday / 22 / Sep / 2017 endOfDay) shouldEqual 1.0
      days(Friday / 22 / Sep / 2017 startOfDay, Monday / 25 / Sep / 2017 startOfDay) shouldEqual 1.0
      days(Friday / 22 / Sep / 2017 startOfDay, Monday / 25 / Sep / 2017 endOfDay) shouldEqual 2.0
      
      days(Friday / 22 / Sep / 2017 startOfDay, Saturday / 23 / Sep / 2017 startOfDay) shouldEqual 1.0
      days(Friday / 22 / Sep / 2017 startOfDay, Saturday / 23 / Sep / 2017 endOfDay) shouldEqual 1.0
      days(Friday / 22 / Sep / 2017 endOfDay, Saturday / 23 / Sep / 2017 endOfDay) shouldEqual 0.0
      days(Friday / 22 / Sep / 2017 endOfDay, Sunday / 24 / Sep / 2017 endOfDay) shouldEqual 0.0
      days(Friday / 22 / Sep / 2017 endOfDay, Monday / 25 / Sep / 2017 startOfDay) shouldEqual 0.0
      
      days(Saturday / 23 / Sep / 2017 startOfDay, Saturday / 23 / Sep / 2017 endOfDay) shouldEqual 0.0
      days(Saturday / 23 / Sep / 2017 startOfDay, Sunday / 24 / Sep / 2017 endOfDay) shouldEqual 0.0
      days(Saturday / 23 / Sep / 2017 startOfDay, Monday / 25 / Sep / 2017 startOfDay) shouldEqual 0.0
      days(Saturday / 23 / Sep / 2017 startOfDay, Monday / 25 / Sep / 2017 endOfDay) shouldEqual 1.0
    }
    
    "same as (d2 - d1) / 252 when no weekends" in {
      val day = Monday / 18 / Sep / 2017
      val d2 = Friday / 22 / Sep / 2017
      day.to(d2).foreach {
        d1 =>
          Weekdays261.timeBetween(d1.endOfDay, d2.endOfDay) shouldEqual (d2.ordinal - d1.ordinal) / 261.0
      }
    }
    
    "day count doesn't include weekends" in {
      def daysBetween(d1: Day, d2: Day) = Weekdays261.timeBetween(d1.endOfDay, d2.endOfDay) * 261
      val d1 = Monday / 18 / Sep / 2017

      daysBetween(d1, d1) shouldEqual 0.0

      daysBetween(d1, Tuesday / 19 / Sep / 2017) shouldEqual 1.0

      daysBetween(d1, Friday / 22 / Sep / 2017) shouldEqual 4.0

      daysBetween(d1, Saturday / 23 / Sep / 2017) shouldEqual 4.0
      
      daysBetween(d1, Sunday / 24 / Sep / 2017) shouldEqual 4.0
      
      daysBetween(d1, Monday / 25 / Sep / 2017) shouldEqual 5.0
      
      daysBetween(d1, Wednesday / 27 / Sep / 2017) shouldEqual 7.0
      
      // Sunday EOD to Monday EOD is 1.0
      daysBetween(Sunday / 24 / Sep / 2017, Monday / 25 / Sep / 2017) shouldEqual 1.0
      
      daysBetween(Sunday / 24 / Sep / 2017, Tuesday / 26 / Sep / 2017) shouldEqual 2.0
      
      daysBetween(Sunday / 24 / Sep / 2017, Wednesday / 27 / Sep / 2017) shouldEqual 3.0
    }

    "random" in {
      def timeBetween(t1: MarketTime, t2: MarketTime): Double = {
        if (t1 > t2) {
          -timeBetween(t2, t1)
        } else {
          var t = t1
          var days = 0
          while (t != t2) {
            t = t.next()
            if (t.day.isWeekday && t.timeOfDay == TimeOfDay.end) {
              days += 1
            }
          }
          days / 261.0
        }
      }
      
      withRandomGenerator(numTests = 100) {
        rng =>
          val t1 = MarketTime(Day(2017, 3, 1) + rng.nextInt(50), rng.nextThing(Seq(TimeOfDay.start, TimeOfDay.end)))
          val t2 = MarketTime(t1.day + rng.nextInt(500), rng.nextThing(Seq(TimeOfDay.start, TimeOfDay.end)))

          timeBetween(t1, t2) shouldEqual Weekdays261.timeBetween(t1, t2)
      }
    }
  }
}
