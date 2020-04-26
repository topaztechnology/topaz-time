package com.topaz.time

import com.topaz.TopazCodingError
import com.topaz.utils.ParseInt

import scala.util.matching.Regex

case class Quarter(year: Int, quarter: Int) extends DateRange {
  require(year >= 1900 && year < 3000, s"Invalid year: $year")
  require(quarter >= 1 && quarter <= 4, s"Invalid quarter: $quarter")

  def dateRangeType: Quarter.type = Quarter

  @transient lazy val ordinal: Int = Quarter.toOrdinal(this)

  def firstDay: Day = Day(year, quarter * 3 - 2, 1)

  def lastDay: Day = Month(year, quarter * 3).lastDay

  override def toString: String = toPersistentString

  override def toPersistentString: String = f"$year%4d-q$quarter%1d"

  def to(rhs: Quarter): Seq[Quarter] = DateRange.inclusiveRange(this, rhs)
  override def next: Quarter = super.next.asInstanceOf[Quarter]
  override def previous: Quarter = super.previous.asInstanceOf[Quarter]
  override def +(n: Int): Quarter = super.+(n).asInstanceOf[Quarter]
  override def -(n: Int): Quarter = super.-(n).asInstanceOf[Quarter]
  def toContractIdentifierSuffix: String = throw TopazCodingError(s"toContractIdentifierSuffix not implemented for $this, $getClass")
  def toContractNameSuffix: String = throw TopazCodingError(s"toContractNameSuffix not implemented for $this, $getClass")
  def toISO8601String: String = throw TopazCodingError(s"toISO8601String not implemented for $this, $getClass")
}

object Quarter extends DateRangeType {
  val ISOFormat: Regex = """(\d{4})\-[Qq](\d{1})""".r
  val Format: Regex = """[Qq](\d)\-(\d{2,4})""".r

  def apply(m: Month): Quarter = new Quarter(m.year, ((m.monthNumber - 1) / 3) + 1)

  def apply(d: Day): Quarter = apply(d.containingMonth)

  def unapply(str: String): Option[Quarter] = str match {
    case ISOFormat(ParseInt(year), ParseInt(quarter)) => Some(Quarter(year, quarter))
    case Format(ParseInt(quarter), ParseInt(Year(year))) => Some(Quarter(year.ordinal, quarter))
    case _ => None
  }

  def toOrdinal(quarter: Quarter): Int = quarter.year * 4 + (quarter.quarter - 1)

  def fromOrdinal(ordinal: Int): Quarter = Quarter(ordinal / 4, (ordinal % 4) + 1)

  override def containing(d: Day): Quarter = apply(d)
}


