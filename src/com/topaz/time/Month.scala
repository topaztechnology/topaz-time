package com.topaz.time

import com.topaz.time.DayOfWeek.{Friday, Sunday, Wednesday}
import com.topaz.utils.{EitherPimpsMixin, GeneralTopazFail, ParseInt, TopazFail}

import scala.language.implicitConversions
import scala.util.matching.Regex


case class Month private(year: Int, monthNumber: Int)
  extends DateRange with VolPeriod {

  def dateRangeType: Month.type = Month

  @transient lazy val ordinal: Int = Month.toOrdinal(this)

  def firstDay: Day = Day(year, monthNumber, 1)

  def lastDay: Day = Day(year, monthNumber, DaysInMonthCalc(year, monthNumber))

  def containingYear: Year = Year(year)

  override def toString: String = toISO8601String

  def toISO8601String: String = f"$year%4d-$monthNumber%02d"

  def toPersistentString: String = toISO8601String

  def toTwoDigitString: String = f"$monthNumber%02d"

  def toReutersCodeString: String = ReutersCodes.monthToCode(monthNumber) + year
  
  def toShortReutersCodeString: String = ReutersCodes.monthToCode(monthNumber) + year.toString.substring(2, 4)

  def toContractIdentifierSuffix: String = f"${ReutersCodes.monthToCode(monthNumber)}${year % 100}%02d"

  def toContractNameSuffix: String = f"[$year%04d-$monthNumber%02d]"

  def day(d: Int): Day = Day(year, monthNumber, d)

  def first(dayOfWeek: DayOfWeek): Day = firstDay + ((7 + (dayOfWeek.value - firstDay.dayOfWeek.value)) % 7)

  def last(dayOfWeek: DayOfWeek): Day = lastDay - ((7 + (lastDay.dayOfWeek.value - dayOfWeek.value)) % 7)

  def lastFriday: Day = last(Friday)
  
  def lastSunday: Day = last(Sunday)

  def firstWeekday: Day = WeekdayCalendar.dayOrNextBusinessDay(firstDay)

  def lastWeekday: Day = WeekdayCalendar.dayOrPreviousBusinessDay(lastDay)
  
  def firstWednesday: Day = first(Wednesday)

  def thirdWednesday: Day = firstWednesday + 14

  def monthOfYear: MonthOfYear = MonthOfYear.fromMonthNumber(monthNumber)

  def containingHalfMonths: Seq[HalfMonth] = Seq(HalfMonth(year, monthNumber, 1), HalfMonth(year, monthNumber, 2))

  def frontHalf: HalfMonth = HalfMonth(year, monthNumber, 1)
  
  def backHalf: HalfMonth = HalfMonth(year, monthNumber, 2)

  def containingQuarter: Quarter = Quarter(this)

  def thisOrNext(monthOfYear: MonthOfYear): Month = this + ((12 + (monthOfYear.monthNumber - this.monthOfYear.monthNumber)) % 12)

  override def remainder(inclusive: Day): Option[DateRange] = if (inclusive <= firstDay) {
    Some(this)
  } else if (inclusive > lastDay) {
    None
  } else {
    Some(BOM(inclusive))
  }

  override def contains(other: DateRange): Boolean = other match {
    case m: Month => this.ordinal == m.ordinal
    case _ => super.contains(other)
  }
  
  def monthName: String = Month.months(monthNumber - 1)

  override def +(n: Int): Month = super.+(n).asInstanceOf[Month]
  override def -(n: Int): Month = super.-(n).asInstanceOf[Month]
  def -(rhs: Month): Int = ordinal - rhs.ordinal
  def to(rhs: Month): Seq[Month] = DateRange.inclusiveRange(this, rhs)

  override def next: Month = super.next.asInstanceOf[Month]
  override def previous: Month = super.previous.asInstanceOf[Month]

}

object Month extends DateRangeType with EitherPimpsMixin {

  private val firstPermittedYear = 1890
  private val lastPermittedYear = 3000
  private val monthArray: Array[Month] = (firstPermittedYear to lastPermittedYear).flatMap {
    y =>
      (1 to 12).map {
        m =>
          new Month(y, m)
      }
  }.toArray

  def apply(year: Int, monthNumber: Int): Month = {
    require(year >= firstPermittedYear && year <= lastPermittedYear, s"year [$year] not supported")
    require(monthNumber >= 1 && monthNumber <= 12, s"month number [$monthNumber] is invalid")
    monthArray((year - firstPermittedYear) * 12 + monthNumber - 1)
  }

  def apply(day: Day): Month = Month(day.year, day.month)

  val months: Vector[String] = Vector("January", "February", "March", "April", "May", "June", "July",
    "August", "September", "October", "November", "December")

  def unapply(str: String): Option[Month] = MonthParser.unapply(str)

  def parseRange(str: String): Option[(Month, Month)] = {
    str.split("->") match {
      case Array(Month(from), Month(to)) if to > from =>
        Some(from -> to)
      case _ =>
        None
    }
  }

  private val ISOStringFormat = """([\d]{4})\-([\d]{2})""".r

  def fromISO(monthStr: String) = monthStr match {
    case ISOStringFormat(ParseInt(year), ParseInt(month)) => Some(Month(year, month))
    case _ => None
  }
  
  private val ContractIdentifierSuffixFormat = """([FGHJKMNQUVXZ])(\d\d)""".r
  
  def fromContractIdentifierSuffix(monthStr: String) : Either[TopazFail, Month] = monthStr match {
    case ContractIdentifierSuffixFormat(month, ParseInt(year)) =>
      Right(Month(2000 + year, ReutersCodes.codeToMonth(month)))
    case _ => 
      TopazFail(s"Can't parse $monthStr into month")
  }

  def parse(str: String): Month = unapply(str).getOrElse(sys.error("Invalid month: " + str))

  def toOrdinal(month: Month): Int = month.year * 12 + (month.monthNumber - 1)

  def fromOrdinal(ordinal: Int) = Month(ordinal / 12, (ordinal % 12) + 1)

  override def containing(d: Day): Month = d.containingMonth

  override def toString = "Monthly"
}

object MonthParser {

  val Format: Regex = """(\d{4})\-(\d{1,2})""".r
  val FormatMMMYY: Regex = """([a-zA-Z]{3})\-*(\d{2,4})""".r

  def unapply(str: String): Option[Month] = str match {
    case Format(ParseInt(Year(year)), ParseInt(MonthOfYear(month))) =>
      Some(Month(year.year, month.value))
    case FormatMMMYY(MonthOfYear(mmm), ParseInt(Year(year))) =>
      Some(Month(year.year, mmm.value))
    case _ =>
      Month.fromContractIdentifierSuffix(str).toOption
  }
}
object MonthRange {
  def unapply(str: String): Option[(Month, Month)] = Month.parseRange(str)
}

object ReutersCodes {
  // http://en.wikipedia.org/wiki/Reuters_Instrument_Code
  val codeToMonth: Map[String, Int] = Map(
    "F" -> 1,
    "G" -> 2,
    "H" -> 3,
    "J" -> 4,
    "K" -> 5,
    "M" -> 6,
    "N" -> 7,
    "Q" -> 8,
    "U" -> 9,
    "V" -> 10,
    "X" -> 11,
    "Z" -> 12
  )
  val monthToCode: Map[Int, String] = codeToMonth.map {case (k, v) => v -> k}
}

object DaysInMonthCalc {
  def apply(year: Int, month: Int): Int = {
    if (month == 4 || month == 6 || month == 9 || month == 11) {
      30
    } else if (month == 2) {
      if (Year.isLeapYear(year))
        29
      else
        28
    } else {
      31
    }
  }
}
