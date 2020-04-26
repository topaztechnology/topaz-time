package com.topaz.time

import scala.language.implicitConversions

/**
 * Syntactic sugar for constructing date ranges and days in tests
 */
trait DateRangeSugar {

  def Jan: MonthOfYear = MonthOfYear.Jan  /** Saves having to import MonthOfYear._ */
  def Feb: MonthOfYear = MonthOfYear.Feb
  def Mar: MonthOfYear = MonthOfYear.Mar
  def Apr: MonthOfYear = MonthOfYear.Apr
  def May: MonthOfYear = MonthOfYear.May
  def Jun: MonthOfYear = MonthOfYear.Jun
  def Jul: MonthOfYear = MonthOfYear.Jul
  def Aug: MonthOfYear = MonthOfYear.Aug
  def Sep: MonthOfYear = MonthOfYear.Sep
  def Oct: MonthOfYear = MonthOfYear.Oct
  def Nov: MonthOfYear = MonthOfYear.Nov
  def Dec: MonthOfYear = MonthOfYear.Dec

  def Monday: DayOfWeek = DayOfWeek.Monday
  def Tuesday: DayOfWeek = DayOfWeek.Tuesday
  def Wednesday: DayOfWeek = DayOfWeek.Wednesday
  def Thursday: DayOfWeek = DayOfWeek.Thursday
  def Friday: DayOfWeek = DayOfWeek.Friday
  def Saturday: DayOfWeek = DayOfWeek.Saturday
  def Sunday: DayOfWeek = DayOfWeek.Sunday

  import DateRangeSugar.{DayOfWeekAndDay, DayAndMonth}
  /**
   * Allows days to be written as (e.g.) 14 / Feb / 1966
   */
  implicit class DaySugar(dayNo: Int) {
    def /(monthName: MonthOfYear): DayAndMonth = {
      require(dayNo <= 31, "incorrect construction of day of month date range sugar")
      new DayAndMonth(dayNo, monthName)
    }
  }

  /*
   * Allows us to write (e.g.) 'Friday / 10 / Jan / 2014' - which will assert the day of week
   * when constructing the day. Handy for unit tests which rely on a day of week
   */
  implicit class DayOfWeekSugar(dayOfWeek: DayOfWeek) {
    def / (dayNo: Int): DayOfWeekAndDay = new DayOfWeekAndDay(dayOfWeek, dayNo)
  }

  implicit class RichDayList[T <: Day](days: Seq[T]) {
    def dateRange: DateRange = {
      DateRange(days.head, days.last)
    }
  }
}

object DateRangeSugar extends DateRangeSugar {

  class DayAndMonth(dayNo: Int, monthName: MonthOfYear) {
    def /(year: Int): Day = {
      val yearNo = if (year < 100) 2000 + year else year // e.g. 1 / Nov / 13
      Day(yearNo, monthName.monthNumber, dayNo)
    }
  }
  class DayOfWeekAndMonth(dayOfWeek: DayOfWeek, dayNo: Int, monthName: MonthOfYear) {
    def /(year: Int): Day = {
      val yearNo = if (year < 100) 2000 + year else year // e.g. 1 / Nov / 13
      val day = Day(yearNo, monthName.monthNumber, dayNo)
      require(day.dayOfWeek == dayOfWeek, s"$day is a ${day.dayOfWeek} not a $dayOfWeek")
      day
    }
  }

  class DayOfWeekAndDay(val dayOfWeek: DayOfWeek, val dayNo: Int) {
    def /(monthName: MonthOfYear): DayOfWeekAndMonth = {
      new DayOfWeekAndMonth(dayOfWeek, dayNo, monthName)
    }
  }
}
