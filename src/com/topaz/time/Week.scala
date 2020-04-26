package com.topaz.time

import com.topaz.time.DayOfWeek.Monday
import com.topaz.utils.{Cache, ParseInt}

import scala.util.matching.Regex

/**
 * An ISO 8601 week
 * https://en.wikipedia.org/?title=ISO_8601#Week_dates
 */
case class Week(year: Int, isoWeek: Int) extends DateRange with VolPeriod {
  require(year >= 1900 && year < 3000, s"Invalid year: $year")
  require(isoWeek >= 1 && isoWeek <= 53, s"Invalid week: $isoWeek")

  def dateRangeType: Week.type = Week

  @transient lazy val ordinal: Int = Week.toOrdinal(this)

  def firstDay: Day = Week.firstDayOfYear(year) + (isoWeek - 1) * 7

  def lastDay: Day = firstDay + 6

  override def toString: String = toISO8601String

  override def toPersistentString: String = toISO8601String

  def toContractIdentifierSuffix: String = toISO8601String

  def toContractNameSuffix: String = s"[$toISO8601String]"

  def toISO8601String: String = f"$year%4d-W$isoWeek%02d"

  def monday: Day = firstDay
  def tuesday: Day = firstDay + 1
  def wednesday: Day = firstDay + 2
  def thursday: Day = firstDay + 3
  def friday: Day = firstDay + 4
  def saturday: Day = firstDay + 5
  def sunday: Day = firstDay + 6

  def -(rhs: Week): Int = ordinal - rhs.ordinal
  override def +(n: Int): Week = super.+(n).asInstanceOf[Week]
  def to(rhs: Week): Seq[Week] = DateRange.inclusiveRange[Week](this, rhs)

}

object Week extends DateRangeType {
  val Format: Regex = """(\d{4})-[wW](\d{2})""".r

  def apply(day: Day): Week = Week.containing(day)

  def fromExcel(excelDay: Double): Week = Day.fromExcel(excelDay).containingWeek

  def unapply(str: String): Option[Week] = str match {
    case Format(ParseInt(year),ParseInt(week)) => Some(Week(year, week))
    case _ => None
  }

  def parse(str: String): Week = unapply(str).getOrElse(sys.error("Invalid week: " + str))

  private val WEEK_0 = Week(1901, 1)

  def toOrdinal(week: Week): Int = {
    (week.firstDay - WEEK_0.firstDay) / 7
  }

  def fromOrdinal(ordinal: Int): Week = {
    val firstDay = WEEK_0.firstDay + ordinal * 7
    val year = if (firstDay < firstDayOfYear(firstDay.year)) firstDay.year - 1 else firstDay.year 
    val weekNumber = (firstDay - Week(year, 1).firstDay) / 7 + 1
    Week(year, weekNumber)
  }

  def firstDayOfYear(year: Int): Day = {
    Day(year, 1, 4).thisOrPreceding(Monday)
  }

  private val weekContainingCache = Cache.createStaticCache("Week.containing")

  // Weeks start on a Monday
  override def containing(d: Day): Week = weekContainingCache.memoize[Day, Week](d){
    if(d < Week(d.year, 1).firstDay) {
      val week = (d - firstDayOfYear(d.year - 1)) / 7 + 1
      Week(d.year - 1, week)
    } else if(d < Week(d.year + 1, 1).firstDay) {
      val week = (d - firstDayOfYear(d.year)) / 7 + 1
      Week(d.year, week)
    } else {
      val week = (d - firstDayOfYear(d.year + 1)) / 7 + 1
      Week(d.year + 1, week)
    }
  }

  override def toString: String = "Weekly"
}
