package com.topaz.time

import scala.util.matching.Regex

case class HalfMonth(year: Int, month: Int, halfNumber: Int) extends DateRange with VolPeriod {
  require(year >= 1900 && year < 3000, s"Invalid year: $year")
  require(month > 0 && month <= 12, s"Invalid month: $month")
  require(halfNumber == 1 || halfNumber == 2, s"Invalid half: $halfNumber")

  def dateRangeType: HalfMonth.type = HalfMonth

  @transient lazy val ordinal: Int = HalfMonth.toOrdinal(this)

  def firstDay: Day = Day(year, month, 1 + (halfNumber - 1) * 15)

  def lastDay: Day = if (halfNumber == 2) containingMonth.lastDay else Day(year, month, 15)

  def containingMonth: Month = Month(year, month)
  
  def containingQuarter: Quarter = containingMonth.containingQuarter

  def containingYear: Year = Year(year)

  private def halfSymbol = if (halfNumber == 1) "F" else "B"

  override def toString: String = toISO8601String

  override def toPersistentString: String = toISO8601String

  def toContractIdentifierSuffix: String = f"${ReutersCodes.monthToCode(month)}${year % 100}%02d$halfSymbol"

  def toContractNameSuffix: String = f"[$year%04d-$month%02d$halfSymbol]"

  def toISO8601String: String = f"$year%4d-$month%02d$halfSymbol"

  def to(rhs: HalfMonth): Seq[HalfMonth] = DateRange.inclusiveRange(this, rhs)
  override def +(n: Int): HalfMonth = super.+(n).asInstanceOf[HalfMonth]
  override def next: HalfMonth = super.next.asInstanceOf[HalfMonth]
  override def previous: HalfMonth = super.previous.asInstanceOf[HalfMonth]

}

object HalfMonth extends DateRangeType {
  def apply(day: Day): HalfMonth = {
    val halfNumber = if (day.dayNumber <= 15) 1 else 2
    HalfMonth(day.year, day.month, halfNumber)
  }

  val Format: Regex = """(\d{4})\-(\d{1,2})([FBfb])""".r

  def unapply(str: String): Option[HalfMonth] = str match {
    case Format(year, month, halfStr) => 
        Some(HalfMonth(year.toInt, month.toInt, if (halfStr.toLowerCase == "f") 1 else 2))
    case _ => 
        None
  }

  def toOrdinal(halfMonth: HalfMonth): Int = halfMonth.year * 24 + (halfMonth.month - 1) * 2 + halfMonth.halfNumber - 1

  def fromOrdinal(ordinal: Int) = HalfMonth(ordinal / 24, (ordinal % 24) / 2 + 1, ordinal % 2 + 1)

  override def containing(d: Day): HalfMonth = apply(d)
}
