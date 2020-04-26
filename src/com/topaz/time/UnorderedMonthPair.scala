package com.topaz.time

import com.topaz.utils.ParseInt

/**
  * Used as a period key into spread vols
  */
case class UnorderedMonthPair private (months: Set[Month]) extends SpreadVolPeriod {

  def frontMonth: Month = months.min
  def backMonth: Month = months.max
  def sorted: Seq[Month] = months.toSeq.sorted
  def spreadWidth: Int = backMonth.ordinal - frontMonth.ordinal

  override def toString = s"$frontMonth / $backMonth"
}

object UnorderedMonthPair {
  def apply(m1: Month, m2: Month): UnorderedMonthPair = UnorderedMonthPair(Set(m1, m2))

  val UnorderedMonthPairFormat = """([\d]{4})\-([\d]{2})\s*/\s*([\d]{4})\-([\d]{2})""".r

  def unapply(s: String): Option[UnorderedMonthPair] = s match {
    case UnorderedMonthPairFormat(ParseInt(year1), ParseInt(month1), ParseInt(year2), ParseInt(month2)) =>
      Some(UnorderedMonthPair(Month(year1, month1), Month(year2, month2)))
    case _ => None
  }

  val ord: Ordering[UnorderedMonthPair] = (pair1: UnorderedMonthPair, pair2: UnorderedMonthPair) =>
    Integer.compare(pair1.frontMonth.ordinal, pair2.frontMonth.ordinal) match {
      case 0 => Integer.compare(pair1.backMonth.ordinal, pair2.backMonth.ordinal)
      case o => o
    }
}
