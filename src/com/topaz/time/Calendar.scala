package com.topaz.time

import com.topaz.time.DayOfWeek.Friday
import com.topaz.time.MarketTimeCount.Weekdays

trait Calendar {
  def isBusinessDay(day: Day): Boolean

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

  def - (days: Day*) = Calendar.Difference(this, days.toSet)
  def + (days: Day*) = Calendar.Plus(this, days.toSet)

  def nearestBusinessDay(day: Day, direction: Int): Day = {
    var d = day
    while (!isBusinessDay(d))
      d = d + direction
    d
  }

  def nearestNonBusinessDay(day: Day, direction: Int): Day = {
    var d = day
    while (isBusinessDay(d))
      d = d + direction
    d
  }

  /**
    * NB - returns `day` when called with 0 - which isn't necessarily a business day
    */
  def addBusinessDays(day: Day, n: Int): Day = {
    val direction = n.signum  // 0, +1 or -1
    var d = day
    var n_ = n
    while (n_ != 0) {
      d = d + direction
      if (isBusinessDay(d))
        n_ = n_ - direction
    }
    d
  }

  def previousBusinessDay(day: Day) = addBusinessDays(day, -1)
  def nextBusinessDay(day: Day) = addBusinessDays(day, 1)

  def &&(other: Calendar) = new Calendar {
    override def isBusinessDay(day: Day): Boolean = Calendar.this.isBusinessDay(day) && other.isBusinessDay(day)
  }

  def ||(other: Calendar) = new Calendar {
    override def isBusinessDay(day: Day): Boolean = Calendar.this.isBusinessDay(day) || other.isBusinessDay(day)
  }

  def dayOrNextBusinessDay(day: Day): Day = nearestBusinessDay(day, direction = 1)
  def dayOrNextNonBusinessDay(day: Day): Day = nearestNonBusinessDay(day, direction = 1)
  def dayOrPreviousBusinessDay(day: Day): Day = nearestBusinessDay(day, direction = -1)
  def dayOrPreviousNonBusinessDay(day: Day): Day = nearestNonBusinessDay(day, direction = -1)

  def isLastBusinessDayInMonth(day: Day): Boolean = {
    day == dayOrPreviousBusinessDay(day.containingMonth.lastDay)
  }

  def businessDays(period: DateRange): Seq[Day] = period.days.filter(isBusinessDay)

  def businessDaysUntil(from: Day, to: Day): Int = {
    require(from <= to, s"From should be on or before to: $from/$to")
    require(isBusinessDay(to), s"To ($to) needs to be a business day")
    var d = from
    var i = 0
    while(d != to) {
      d = addBusinessDays(d, 1)
      i += 1
    }
    i
  }
}

object Calendar {
  case class Difference(cal: Calendar, excludeDays: Set[Day]) extends Calendar {
    def isBusinessDay(day: Day) =
      cal.isBusinessDay(day) && !excludeDays.contains(day)
  }
  case class Plus(cal: Calendar, includeDays: Set[Day]) extends Calendar {
    def isBusinessDay(day: Day) =
      cal.isBusinessDay(day) || includeDays.contains(day)
  }
}

case class SimpleCalendar(holidays: Set[Day]) extends Calendar {
  def isBusinessDay(day: Day) = day.isWeekday && !holidays.contains(day)

  override def businessDaysBetween(d1: Day, d2: Day): Int = {
    // just an optimisation over the base method
    if (d1 > d2) {
      -businessDaysBetween(d2, d1)
    } else {
      val sumWeekDays = Weekdays.weekDaysBetween(d1, d2)
      // Don't double count holidays that are also weekends. We don't
      // count the last day in sumWeekDays, so don't remove it if we happen to have
      // a holiday on that day either (d2 > d)
      val holidaysBetween = holidays.count(d => d.isWeekday && d1 <= d && d2 > d)

      sumWeekDays - holidaysBetween
    }
  }
}

object EveryDayCalendar extends Calendar {
  def isBusinessDay(day: Day) = true
}

object WeekdayCalendar extends Calendar {
  def isBusinessDay(day: Day) = day.isWeekday
}

object TargetCalendar extends Calendar {
  def isBusinessDay(day: Day) = {
    day.isWeekday &&
      !day.isNewYearsDay &&
      !day.isGoodFriday &&
      !day.isLabourDay &&
      !day.isEasterMonday &&
      !day.isChristmasDay &&
      !day.isBoxingDay
  }
}

/**
  * Useful for Coal markets which often have prices published only on Fridays. They can even have
  * a price published if that Friday is a holiday for the market's price publisher.
  */
object FridayCalendar extends Calendar {
  def isBusinessDay(day: Day) = day.dayOfWeek == Friday
}
