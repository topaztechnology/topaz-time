package com.topaz.time

import com.topaz.TopazCodingError
import com.topaz.utils.ParseInt

import scala.util.matching.Regex

case class Year(year: Int) extends DateRange {
  require(Year.isValidYearNumber(year), s"Invalid year: $year")

  def dateRangeType: Year.type = Year

  @transient lazy val ordinal: Int = Year.toOrdinal(this)

  def firstDay = Day(year, 1, 1)

  def lastDay = Day(year, 12, 31)

  def quarters: Seq[Quarter] = (1 to 4).map (q => Quarter(year, q))

  override def toString: String = toPersistentString

  def toPersistentString: String = f"$year%4d"

  def toTwoDigitString: String = toPersistentString.drop(2)

  def isLeapYear: Boolean = Year.isLeapYear(year)

  def to(rhs: Year): Seq[Year] = DateRange.inclusiveRange(this, rhs)
  override def next: Year = super.next.asInstanceOf[Year]
  override def previous: Year = super.previous.asInstanceOf[Year]
  override def +(n: Int): Year = super.+(n).asInstanceOf[Year]
  override def -(n: Int): Year = super.-(n).asInstanceOf[Year]
  def -(rhs: Year): Int = ordinal - rhs.ordinal
  def toContractIdentifierSuffix: String = throw TopazCodingError(s"toContractIdentifierSuffix not implemented for $this, $getClass")
  def toContractNameSuffix: String = throw TopazCodingError(s"toContractNameSuffix not implemented for $this, $getClass")
  def toISO8601String: String = throw TopazCodingError(s"toISO8601String not implemented for $this, $getClass")
}

object Year extends DateRangeType {
  val ISOFormat: Regex = """(\d{4})""".r
  val Format: Regex = """cal\-(\d{2,4})""".r

  def unapply(str: String): Option[Year] = str match {
    case ISOFormat(ParseInt(year)) if isValidYearNumber(year)  => Some(Year(year))
    case Format(ParseInt(year)) if isValidYearNumber(year) => Some(Year(year))
    case Format(ParseInt(year)) if isValidYearNumber(year + 2000) => Some(Year(year + 2000))
    case _ => None
  }

  def unapply(arg: Double): Option[Year] = unapply(arg.toInt)
  
  def unapply(arg: Int): Option[Year] = {
    if (isValidYearNumber(arg))
      Some(Year(arg))
    else if (isValidYearNumber(arg + 2000))
      Some(Year(arg + 2000))
    else
      None
  }

  def toOrdinal(year: Year): Int = year.year

  def fromOrdinal(ordinal: Int) = Year(ordinal)

  def containing(d: Day): Year = d.containingYear
  
  def isLeapYear(year: Int): Boolean = {
    (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)
  }
  def isValidYearNumber(year: Int): Boolean = {
    year >= 1900 && year < 3000
  }

  override def toString: String = "Yearly"
}


