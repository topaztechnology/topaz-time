package com.topaz.time

import com.topaz.utils.{NamedIntEnum, NamedIntEnumEntry}

import scala.collection.immutable

sealed abstract class MarketTimeCountType(override val name: String, override val value: Int)
  extends NamedIntEnumEntry {
  override def toString: String = name
}

object MarketTimeCountType extends NamedIntEnum[MarketTimeCountType] {
  def values: immutable.IndexedSeq[MarketTimeCountType] = findValues

  case object Act365Type extends MarketTimeCountType("Act365", 1)

  case object Weekdays261Type extends MarketTimeCountType("Weekdays261", 2)

  case object Weekdays252Type extends MarketTimeCountType("Weekdays252", 3)

  case object BusinessDaysType extends MarketTimeCountType("BusinessDays", 4)
}

sealed trait MarketTimeCount {
  def typ: MarketTimeCountType

  def timeBetween(t1: MarketTime, t2: MarketTime): Double

  def calculationDescription(t1: MarketTime, t2: MarketTime): String
}

object MarketTimeCount {

  case object Act365 extends MarketTimeCount {
    def typ: MarketTimeCountType = MarketTimeCountType.Act365Type

    def timeBetween(t1: MarketTime, t2: MarketTime): Double = {
      daysBetween(t1, t2) / 365.0
    }

    private def daysBetween(t1: MarketTime, t2: MarketTime) = {
      t2.day.ordinal - t1.day.ordinal + t1.timeOfDay.daysBetween(t2.timeOfDay)
    }

    def calculationDescription(t1: MarketTime, t2: MarketTime): String =
      s"Act365($t1, $t2, daysBetween = ${daysBetween(t1, t2)})"
  }

  sealed trait Weekdays extends MarketTimeCount {
    def basis: Int

    private def daysBetween(t1: MarketTime, t2: MarketTime): Int = {
      if (t1 > t2) {
        -daysBetween(t2, t1)
      } else if (t2.day.isWeekend) {
        daysBetween(t1, MarketTime(t2.day.nextWeekday, TimeOfDay.start))
      } else if (t1.day.isWeekend) {
        daysBetween(MarketTime(t1.day.previousWeekday, TimeOfDay.end), t2)
      } else {
        var sumWeekDays = Weekdays.weekDaysBetween(t1.day, t2.day)
        sumWeekDays += t1.timeOfDay.daysBetween(t2.timeOfDay)
        sumWeekDays
      }
    }

    def timeBetween(t1: MarketTime, t2: MarketTime): Double = {
      val sumWeekDays = daysBetween(t1, t2)
      sumWeekDays / basis.toDouble
    }

    def calculationDescription(t1: MarketTime, t2: MarketTime): String =
      s"Act$basis($t1, $t2, daysBetween = ${daysBetween(t1, t2)})"
  }

  object Weekdays {
    def weekDaysBetween(d1: Day, d2: Day): Int = {
      if (d1 > d2) {
        -weekDaysBetween(d2, d1)
      } else if (d2.isWeekend) {
        weekDaysBetween(d1, d2.nextWeekday)
      } else if (d1.isWeekend) {
        weekDaysBetween(d1.nextWeekday, d2)
      } else {
        var sumWeekDays = 0
        var d = d1
        while (d.dayOfWeek != d2.dayOfWeek) {
          d = d.nextWeekday
          sumWeekDays += 1
        }
        sumWeekDays += (d2 - d) / 7 * 5
        sumWeekDays
      }
    }
  }

  case object Weekdays261 extends Weekdays {
    def typ: MarketTimeCountType = MarketTimeCountType.Weekdays261Type

    val basis: Int = 261
  }

  case object Weekdays252 extends Weekdays {
    def typ: MarketTimeCountType = MarketTimeCountType.Weekdays252Type

    val basis: Int = 252
  }

  case class BusinessDaysMarketTime(businessDayCalculation: BusinessDayCalculation) extends MarketTimeCount {
    def typ: MarketTimeCountType = MarketTimeCountType.BusinessDaysType

    private def daysBetween(t1: MarketTime, t2: MarketTime): Int = {
      if (t1 > t2) {
        -daysBetween(t2, t1)
      } else if (!businessDayCalculation.isBusinessDay(t2.day)) {
        daysBetween(t1, MarketTime(businessDayCalculation.nextBusinessDay(t2.day), TimeOfDay.start))
      } else if (!businessDayCalculation.isBusinessDay(t1.day)) {
        daysBetween(MarketTime(businessDayCalculation.previousBusinessDay(t1.day), TimeOfDay.end), t2)
      } else {
        var businessDays = businessDayCalculation.businessDaysBetween(t1.day, t2.day)
        businessDays += t1.timeOfDay.daysBetween(t2.timeOfDay)
        businessDays
      }
    }

    def timeBetween(t1: MarketTime, t2: MarketTime): Double = {
      val businessDays = daysBetween(t1, t2)
      businessDays / 252.0
    }

    def calculationDescription(t1: MarketTime, t2: MarketTime): String =
      s"BusinessDays(${businessDayCalculation.name}, $t1, $t2, daysBetween = ${daysBetween(t1, t2)})"
  }
}

