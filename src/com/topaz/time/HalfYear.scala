package com.topaz.time

import com.topaz.TopazCodingError
import com.topaz.utils.ParseInt

case class HalfYear(year: Int, halfNumber: Int) extends DateRange {
  require(year >= 1900 && year < 3000, s"Invalid year: $year")
  require(halfNumber == 1 || halfNumber == 2, s"Invalid half: $halfNumber")

  def dateRangeType: HalfYear.type = HalfYear

  @transient lazy val ordinal: Int = HalfYear.toOrdinal(this)

  def firstDay: Day = Day(year, (halfNumber - 1) * 6 + 1, 1)

  def lastDay: Day = Month(year, halfNumber * 6).lastDay

  override def toString: String = toPersistentString

  override def toPersistentString: String = f"$year%4d-${if (halfNumber == 1) "F" else "B"}"
  def toContractIdentifierSuffix: String = throw TopazCodingError(s"toContractIdentifierSuffix not implemented for $this, $getClass")
  def toContractNameSuffix: String = throw TopazCodingError(s"toContractNameSuffix not implemented for $this, $getClass")
  def toISO8601String: String = throw TopazCodingError(s"toISO8601String not implemented for $this, $getClass")
}

object HalfYear extends DateRangeType {

  def apply(day: Day): HalfYear = {
    val halfNumber = if (day.month <= 6) 1 else 2
    HalfYear(day.year, halfNumber)
  }

  private val ISOFormat = """(\d{4})\-([FBfb])""".r
  private val Format1 = """([12])h\-(\d{2})""".r
  private val Format2 = """(\d{4})\-([12])[hH]""".r

  def unapply(str: String): Option[HalfYear] = str match {
    case ISOFormat(year, halfStr) =>
      Some(HalfYear(year.toInt, if (halfStr.toLowerCase == "f") 1 else 2))
    case Format1(ParseInt(half), ParseInt(Year(year))) => Some(HalfYear(year.ordinal, half))
    case Format2(ParseInt(Year(year)), ParseInt(half)) => Some(HalfYear(year.ordinal, half))
    case _ =>
      None
  }

  def toOrdinal(halfYear: HalfYear): Int = halfYear.year * 2 + halfYear.halfNumber - 1

  def fromOrdinal(ordinal: Int) = HalfYear(ordinal / 2, ordinal % 2 + 1)

  override def containing(d: Day): HalfYear = apply(d)
}
