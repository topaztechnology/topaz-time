package com.topaz.time

import com.topaz.utils.{NamedIntEnum, NamedIntEnumEntry}

import scala.collection.immutable

sealed abstract class InterestRateDayCount(override val name: String, override val value: Int) extends NamedIntEnumEntry {
  def timeBetween(d1: Day, d2: Day): Double

  def calculationDescription(d1: Day, d2: Day): String
}

object InterestRateDayCount extends NamedIntEnum[InterestRateDayCount] {
  val values: immutable.IndexedSeq[InterestRateDayCount] = findValues

  case object Act365 extends InterestRateDayCount("Act365", 1) {
    def timeBetween(d1: Day, d2: Day): Double = (d2.ordinal - d1.ordinal) / 365.0

    def calculationDescription(d1: Day, d2: Day): String = s"Act365($d1, $d2, daysBetween = ${d2.ordinal - d1.ordinal})"
  }

  case object Act360 extends InterestRateDayCount("Act360", 2) {
    def timeBetween(d1: Day, d2: Day): Double = (d2.ordinal - d1.ordinal) / 360.0

    def calculationDescription(d1: Day, d2: Day): String = s"Act360($d1, $d2, daysBetween = ${d2.ordinal - d1.ordinal})"
  }

  case object ActAct extends InterestRateDayCount("ActAct", 3) {
    /* Days in leap year / 366.0 + days not in leap year / 365.0
     * Including first day, excluding last day.
     */
    def timeBetween(d1: Day, d2: Day): Double = {
      if (d1 > d2)
        -timeBetween(d2, d1)
      else {
        var firstDay = d1
        var sum = 0.0
        while (firstDay < d2) {
          val y = firstDay.containingYear
          val lastDay = y.lastDay + 1 min d2
          val diff = lastDay - firstDay
          if (y.isLeapYear)
            sum += diff / 366.0
          else
            sum += diff / 365.0
          firstDay = lastDay
        }
        sum
      }
    }

    def calculationDescription(d1: Day, d2: Day): String = s"Act360($d1, $d2)"
  }

  case object _30_360 extends InterestRateDayCount("30/360", 4) {
    def timeBetween(day1: Day, day2: Day): Double = {
      val days = daysBetween(day1, day2)
      days / 360.0
    }

    private def daysBetween(day1: Day, day2: Day): Int = {
      val d1 = day1.dayNumber min 30
      val d2 = if (day2.dayNumber == 31 && d1 == 30)
        30
      else
        day2.dayNumber

      (day2.year - day1.year) * 360 + (day2.month - day1.month) * 30 + (d2 - d1)
    }

    def calculationDescription(d1: Day, d2: Day): String = s"30_360($d1, $d2, daysBetween = ${daysBetween(d1, d2)})"
  }

  case class BusinessDaysDayCount(businessDayCalculation: BusinessDayCalculation)
    extends InterestRateDayCount("BusinessDays", 5) {

    def timeBetween(d1: Day, d2: Day): Double = {
      val businessDays = businessDaysBetween(d1, d2)
      businessDays / 252.0
    }

    def businessDaysBetween(d1: Day, d2: Day): Int =
      if (d1 > d2) {
        -businessDaysBetween(d2, d1)
      } else {
        val businessDays = businessDayCalculation.businessDaysBetween(d1, d2)
        businessDays
      }

    def calculationDescription(d1: Day, d2: Day): String = {
      s"BusinessDays(${businessDayCalculation.name}, $d1, $d2, daysBetween = ${businessDaysBetween(d1, d2)})"
    }
  }

  case object Weekday252 extends InterestRateDayCount("Weekday252", 6) {
    private val dc = BusinessDaysDayCount(BusinessDayCalculation.weekdaysBusinessDayCalculation)

    def timeBetween(d1: Day, d2: Day): Double = dc.timeBetween(d1, d2)

    def calculationDescription(d1: Day, d2: Day): String = {
      s"Weekday252($d1, $d2, daysBetween = ${dc.businessDaysBetween(d1, d2)})"
    }
  }
}
