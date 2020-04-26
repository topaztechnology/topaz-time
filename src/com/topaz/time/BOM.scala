package com.topaz.time

import com.topaz.TopazCodingError

import scala.util.matching.Regex

/**
 * Balance of Month from `fromDay` to the end of the month (inclusive)
 */
case class BOM(fromDay: Day) extends DateRange {
  def firstDay: Day = fromDay

  def lastDay: Day = containingMonth.lastDay

  def containingMonth: Month = fromDay.containingMonth

  override def toString: String = toPersistentString

  override def toPersistentString: String = s"BOM(${fromDay.toISO8601String})"

  def dateRangeType: DateRangeType = BOM

  def ordinal: Int = fromDay.ordinal

  def toContractIdentifierSuffix: String = throw TopazCodingError(s"toContractIdentifierSuffix not implemented for $this, $getClass")
  def toContractNameSuffix: String = throw TopazCodingError(s"toContractNameSuffix not implemented for $this, $getClass")
  def toISO8601String: String = throw TopazCodingError(s"toISO8601String not implemented for $this, $getClass")
}

object BOM extends DateRangeType {
  val Format: Regex = """BOM\((.+)\)""".r

  def unapply(str: String): Option[BOM] = str match {
    case Format(Day(day)) =>
        Some(new BOM(day))
    case _ =>
        None
  }

  def containing(d: Day): BOM = BOM(d)

  def fromOrdinal(ordinal: Int): DateRange = {
    BOM(Day.fromOrdinal(ordinal))
  }
}
