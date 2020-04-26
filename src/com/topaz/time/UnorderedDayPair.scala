package com.topaz.time

import com.topaz.TopazCodingError
import com.topaz.utils.ParseInt

case class UnorderedDayPair private(private val days: Set[Day]) extends SpreadVolPeriod {
  def frontDay: Day = days.toSeq.min
  def backDay: Day = days.toSeq.max
  def spreadWidth: Int = backDay.ordinal - frontDay.ordinal

  override def toString = s"$frontDay / $backDay"
}

object UnorderedDayPair {
  def apply(d1: Day, d2: Day): UnorderedDayPair = {
    TopazCodingError.require(d1 != d2, s"Days in UnorderedDayPair must be distinct, got $d1/$d2")
    UnorderedDayPair(Set(d1, d2))
  }

  val UnorderedDayPairFormat = """(\d{4})\-(\d{2})\-(\d{2})\/(\d{4})\-(\d{2})\-(\d{2})""".r

  def unapply(s: String): Option[UnorderedDayPair] = s match {
    case UnorderedDayPairFormat(
      ParseInt(year1), ParseInt(month1), ParseInt(day1),
      ParseInt(year2), ParseInt(month2), ParseInt(day2)
    ) => Some(UnorderedDayPair(Day(year1, month1, day1), Day(year2, month2, day2)))
    case _ => None
  }
}
