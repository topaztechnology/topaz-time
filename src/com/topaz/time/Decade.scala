package com.topaz.time

import com.topaz.TopazCodingError

import scala.util.matching.Regex

case class Decade(year: Int, month: Int, decadeNumber: Int) extends DateRange {
  require(year >= 1900 && year < 3000, s"Invalid year: $year")
  require(month > 0 && month <= 12, s"Invalid month: $month")
  require(decadeNumber > 0 && decadeNumber <= 3, s"Invalid decade: $decadeNumber")

  def dateRangeType: Decade.type = Decade

  @transient lazy val ordinal: Int = Decade.toOrdinal(this)

  def firstDay = Day(year, month, 1 + (decadeNumber - 1) * 10)

  def lastDay: Day = if (decadeNumber == 3) containingMonth.lastDay else Day(year, month, decadeNumber * 10)

  def containingMonth = Month(year, month)

  override def toString: String = toPersistentString

  override def toPersistentString: String = f"$year%4d-$month%02d:$decadeNumber%01d"

  override def +(n: Int): Decade = super.+(n).asInstanceOf[Decade]
  override def next: Decade = super.next.asInstanceOf[Decade]
  override def previous: Decade = super.previous.asInstanceOf[Decade]
  def to(rhs: Decade): Seq[Decade] = DateRange.inclusiveRange[Decade](this, rhs)

  override def hashCode(): Int = ordinal
  def toContractIdentifierSuffix: String = throw TopazCodingError(s"toContractIdentifierSuffix not implemented for $this, $getClass")
  def toContractNameSuffix: String = throw TopazCodingError(s"toContractNameSuffix not implemented for $this, $getClass")
  def toISO8601String: String = throw TopazCodingError(s"toISO8601String not implemented for $this, $getClass")
}

object Decade extends DateRangeType {
  def apply(day: Day): Decade = {
    val dayNumber = if (day.dayNumber < 11) 1 else if (day.dayNumber < 21) 2 else 3
    Decade(day.year, day.month, dayNumber)
  }

  val Format: Regex = """(\d{4})\-(\d{1,2}):(\d)""".r

  def unapply(str: String): Option[Decade] = str match {
    case Format(year, month, decade) => Some(Decade(year.toInt, month.toInt, decade.toInt))
    case _ => None
  }

  def toOrdinal(decade: Decade): Int = decade.year * 36 + (decade.month - 1) * 3 + decade.decadeNumber - 1

  def fromOrdinal(ordinal: Int) = Decade(ordinal / 36, (ordinal % 36) / 3 + 1, ordinal % 3 + 1)

  override def containing(d: Day): Decade = apply(d)
}
