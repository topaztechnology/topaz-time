package com.topaz.time

import com.topaz.utils.{NamedIntEnum, NamedIntEnumEntry}

sealed abstract class MonthOfYear(val value: Int, val name: String) extends NamedIntEnumEntry {
  def monthNumber: Int = value

  def /(year: Int): Month = {
    val yearNo = if (year < 100) 2000 + year else year
    Month(yearNo, monthNumber)
  }
  
  override def equals(obj: scala.Any): Boolean = obj match {
    case other: MonthOfYear => value == other.value
    case _ => false
  }

  override def hashCode(): Int = value

  override def toString: String = name

  def next: MonthOfYear = MonthOfYear.fromMonthNumber(value % 12 + 1)
}

object MonthOfYear extends NamedIntEnum[MonthOfYear] {
  val values = findValues

  override protected def caseSensitive: Boolean = false

  case object Jan extends MonthOfYear(1, "Jan")
  case object Feb extends MonthOfYear(2, "Feb")
  case object Mar extends MonthOfYear(3, "Mar")
  case object Apr extends MonthOfYear(4, "Apr")
  case object May extends MonthOfYear(5, "May")
  case object Jun extends MonthOfYear(6, "Jun")
  case object Jul extends MonthOfYear(7, "Jul")
  case object Aug extends MonthOfYear(8, "Aug")
  case object Sep extends MonthOfYear(9, "Sep")
  case object Oct extends MonthOfYear(10, "Oct")
  case object Nov extends MonthOfYear(11, "Nov")
  case object Dec extends MonthOfYear(12, "Dec")

  def fromMonthNumber(monthNumber: Int): MonthOfYear = withValue(monthNumber)

  val Months: IndexedSeq[MonthOfYear] = (1 to 12).map(fromMonthNumber)
  
  def unapply(name: String): Option[MonthOfYear] = maybeWithName(name)
  def unapply(i: Int): Option[MonthOfYear] = withValueOpt(i)
}
