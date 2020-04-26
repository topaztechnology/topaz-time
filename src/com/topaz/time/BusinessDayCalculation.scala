package com.topaz.time

import com.topaz.time.MarketTimeCount.Weekdays


trait BusinessDayCalculation {
  def name: String
  
  def businessDaysBetween(d1: Day, d2: Day): Int

  def isBusinessDay(day: Day): Boolean

  def nextBusinessDay(day: Day): Day

  def previousBusinessDay(day: Day): Day
}

object BusinessDayCalculation {
  val weekdaysBusinessDayCalculation: BusinessDayCalculation = new BusinessDayCalculation {
    def name: String = "Weekdays"

    def businessDaysBetween(d1: Day, d2: Day): Int = Weekdays.weekDaysBetween(d1, d2)

    def isBusinessDay(day: Day): Boolean = day.isWeekday

    def nextBusinessDay(day: Day): Day = day.nextWeekday

    def previousBusinessDay(day: Day): Day = day.previousWeekday
  }
}
