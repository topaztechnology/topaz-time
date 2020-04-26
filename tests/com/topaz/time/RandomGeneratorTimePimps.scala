package com.topaz.time

import com.topaz.TopazCodingError
import com.topaz.utils.RandomGeneratorPimps
import org.apache.commons.math3.random.RandomGenerator

trait RandomGeneratorTimePimps extends RandomGeneratorPimps {
  implicit class PimpedTimeRandomGenerator(rng: RandomGenerator) {
    def nextDay(): Day = Day(2017, 1, 1) + rng.nextInt(500)

    def nextDayInRange(start: Day, end: Day): Day = {
      require(start <= end, s"nextDayInRange start must be before end: $start <= $end")
      if (start == end)
        start
      else
        start + rng.nextInt(end - start)
    }

    def nextDayInRange(dateRange: DateRange): Day = nextDayInRange(dateRange.firstDay, dateRange.lastDay)

    def nextMarketTimeIn(dateRange: DateRange): MarketTime = {
      val day = nextDayInRange(dateRange)
      if (rng.isHeads())
        day.startOfDay
      else
        day.endOfDay
    }

    def randomDay(containingRange: DateRange): Day = {
      containingRange.firstDay + rng.nextInt(containingRange.size)
    }
    def randomDateRange(containingRange: DateRange, minLength: Int = 1, maxLength:Int = -1): DateRange = {
      val maxLengthToUse = if (maxLength == -1) containingRange.size else maxLength
      TopazCodingError.require(
        0 <= minLength && minLength <= maxLengthToUse && maxLengthToUse <= containingRange.size,
        s"Inconsistent random date range criteria, $containingRange, min $minLength, max $maxLengthToUse"
      )
      val d1 = randomDay(DateRange(
        containingRange.firstDay,
        containingRange.lastDay - (minLength - 1))
      )
      val d2 = (d1 + rng.nextInt(minLength - 1, maxLengthToUse)).min(containingRange.lastDay)
      DateRange(d1, d2)
    }

    def randomMonth(containingRange: DateRange): Month = {
      randomDay(containingRange).containingMonth
    }

  }
}
