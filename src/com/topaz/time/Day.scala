package com.topaz.time

import java.util.{Calendar => JCalendar}

import com.topaz.time.DayOfWeek.Friday
import com.topaz.utils.{GeneralTopazFail, ParseInt, TopazFail}

import scala.language.implicitConversions
import java.time.LocalDate


case class Day private(@transient year: Int, @transient month: Int, @transient dayNumber: Int)
  extends DateRange with VolPeriod
{

  def dateRangeType: Day.type = Day

  val ordinal: Int = Day.julianDayNumber(year, month, dayNumber)

  def firstDay: Day = this

  def lastDay: Day = this

  def lastDayOfMonth: Day = containingMonth.lastDay
  def firstDayOfMonth: Day = containingMonth.firstDay
  def firstDayOfYear: Day = containingYear.firstDay
  def isLastDayOfMonth: Boolean = this == lastDayOfMonth
  def isFirstDayOfMonth: Boolean = this == firstDayOfMonth
  def isFirstDayOfYear: Boolean = this == firstDayOfYear
  def containingWeek = Week(this)

  def containingHalfMonth: HalfMonth = HalfMonth.containing(this)
  def containingMonth = Month(year, month)
  def containingQuarter: Quarter = Quarter.containing(this)

  def containingGasSeason = GasSeason.containing(this)
  def containingYear = Year(year)
  def containingGasYear: GasStorageYear = GasStorageYear.containing(this)

  def containing(typ: DateRangeType): DateRange = typ.containing(this)

  def dayOfWeek: DayOfWeek = DayOfWeek.withValue(ordinal % 7)

  override def +(n: Int): Day = super.+(n).asInstanceOf[Day]
  override def -(n: Int): Day = super.-(n).asInstanceOf[Day]
  def -(rhs: Day): Int = ordinal - rhs.ordinal
  def nextDay: Day = Day.fromOrdinal(ordinal + 1)
  def previousDay: Day = Day.fromOrdinal(ordinal - 1)
  def thisOrPreviousFriday: Day = {
    this - (dayOfWeek.value - 4).abs
  }

  def addBusinessDays(calendar: Calendar, n: Int): Day = calendar.addBusinessDays(this, n)
  def thisOrNextBusinessDay(calendar: Calendar): Day = calendar.dayOrNextBusinessDay(this)
  def thisOrNextNonBusinessDay(calendar: Calendar): Day = calendar.dayOrNextNonBusinessDay(this)
  def thisOrPreviousBusinessDay(calendar: Calendar): Day = calendar.dayOrPreviousBusinessDay(this)
  def thisOrPreviousNonBusinessDay(calendar: Calendar): Day = calendar.dayOrPreviousNonBusinessDay(this)
  def previousBusinessDay(calendar: Calendar): Day = calendar.previousBusinessDay(this)

  def max(rhs: Day): Day = {
    if (ordinal > rhs.ordinal)
      this
    else
      rhs
  }
  def min(rhs: Day): Day = {
    if (ordinal < rhs.ordinal)
      this
    else
      rhs
  }
  def thisOrNextWeekday: Day = if(isWeekday) this else nextWeekday
  def thisOrPreviousWeekday: Day = if(isWeekday) this else previousWeekday

  def nextWeekday: Day = addWeekdays(1)
  def previousWeekday: Day = addWeekdays(-1)

  def addWeekdays(n: Int): Day = {
    addDays(n, _.isWeekday)
  }
  
  def addDays(n: Int, validDay: Day => Boolean): Day = {
    var d = this
    var n_ = n.abs
    while (n_ > 0) {
      d = d + n / n.abs
      if (validDay(d))
        n_ = n_ - 1
    }
    d
  }

  def thisOrPreceding(dayOfWeek: DayOfWeek): Day = if (this.dayOfWeek == dayOfWeek)
    this
  else
    moveToPreceding(dayOfWeek)

  def moveToPreceding(precedingDayOfWeek: DayOfWeek): Day = 
    this - precedingDayOfWeek.numDaysUntilNext(dayOfWeek)

  def thisOrNext(dayOfWeek: DayOfWeek): Day = if (this.dayOfWeek == dayOfWeek)
    this
  else
    moveToNext(dayOfWeek)

  def moveToNext(dow: DayOfWeek): Day = 
    this + dayOfWeek.numDaysUntilNext(dow)

  /** Adds months using the most common interest rate swap convention, namely :-
    * - return the last day of the month when the day number would otherwise be too large.
    * - return the last day of the month when this day is the last day of its month
    */
  def addMonths(n: Int): Day = {
    val m = containingMonth + n
    if (this.isLastDayOfMonth)
      m.lastDay
    else {
      if (dayNumber > m.lastDay.dayNumber)
        m.lastDay
      else
        Day(m.year, m.monthNumber, dayNumber)
    }
  }

  def addWeeks(n: Int): Day = this + (n * 7)

  override def toString: String = toISO8601String

  def toISO8601String: String = f"$year%4d-$month%02d-$dayNumber%02d"

  def toLocalDate: LocalDate = LocalDate.of(year, month, dayNumber)

  def toContractIdentifierSuffix: String = f"$dayNumber%02d$month%02d${year % 100}%02d"

  def toContractNameSuffix = f"[$year%04d-$month%02d-$dayNumber%02d]"
  
  def toLongString: String = s"$dayOfWeek / $dayNumber / ${containingMonth.monthName} / $year"

  def toPersistentString: String = toISO8601String

  def toTwoDigitString: String = f"$dayNumber%02d"

  def toExcel: Double = (this.ordinal - Day.excelZeroDay).doubleValue

  private def readResolve(): Object = Day.dayFromJulianDayNumber(ordinal)
  
  
  def isWeekend: Boolean = (ordinal % 7) > 4

  def isWeekday: Boolean = !isWeekend

  def isMonday: Boolean = (ordinal % 7) == 0
  def isTuesday: Boolean = (ordinal % 7) == 1
  def isWednesday: Boolean = (ordinal % 7) == 2
  def isThursday: Boolean = (ordinal % 7) == 3
  def isFriday: Boolean = (ordinal % 7) == 4
  def isSaturday: Boolean = (ordinal % 7) == 5
  def isSunday: Boolean = (ordinal % 7) == 6
  
  def isEasterSunday: Boolean = EasterCalc.easterSunday(year) == this
  def isEasterMonday: Boolean = EasterCalc.easterSunday(year) + 1 == this

  def isGoodFriday: Boolean = EasterCalc.easterSunday(year).moveToPreceding(Friday) == this

  def isChristmasDay: Boolean = month == 12 && dayNumber == 25
  def isBoxingDay: Boolean = month == 12 && dayNumber == 26
  def isNewYearsDay: Boolean = month == 1 && dayNumber == 1
  def isLabourDay: Boolean = month == 5 && dayNumber == 1

  def endOfDay = MarketTime(this, TimeOfDay.end)
  def startOfDay = MarketTime(this, TimeOfDay.start)

  def to(rhs: Day): IndexedSeq[Day] = {
    DateRange.inclusiveRange(this, rhs)
  }

  def to(rhs: Day, step: Int): IndexedSeq[Day] = {
    (ordinal to(rhs.ordinal, step)).map(Day.fromOrdinal)
  }

  def until(rhs: Day): IndexedSeq[Day] = {
    DateRange.inclusiveRange(this, rhs - 1)
  }

  def until(rhs: Day, step: Int): IndexedSeq[Day] = {
    (ordinal until(rhs.ordinal, step)).map(Day.fromOrdinal)
  }

}

object Day extends DateRangeType {
  private val firstPermittedJuliandayNumber = julianDayNumber(1890, 1, 1) // before excel day 0 and epoch
  private val lastPermittedJuliandayNumber = julianDayNumber(2200, 12, 31)
  private val excelZeroDay = julianDayNumber(1899, 12, 30)
  private val earliestPermittedExcelDay = {
    // to help us distinguish days sent as doubles from excel from numeric years (#167707857)
    // otherwise a cell with 2019 would parse as a valid day, but in 1905.
    // picking a cut-off where it's unlikely the user really meant such an early date.
    julianDayNumber(1910, 1, 1) - excelZeroDay
  }
  /** Flyweights to limit memory usage of Day objects */
  private val dayArray = {
    val start = firstPermittedJuliandayNumber
    val end = lastPermittedJuliandayNumber
    val array = new Array[Day](end - start + 1)
    for (j <- start to end) {
      val (y, m, d) = julianDayNumberToYearMonthDay(j)
      array(j - firstPermittedJuliandayNumber) = new Day(y, m, d)
    }
    array
  }
  
  val firstPermittedDay: Day = dayFromJulianDayNumber(firstPermittedJuliandayNumber)
  val lastPermittedDay: Day = dayFromJulianDayNumber(lastPermittedJuliandayNumber)

  def dayFromJulianDayNumber(jdn: Int): Day = {
    val ndx = dayIndex(jdn)
    require(ndx < dayArray.length && ndx >= 0, "Julian day number " + jdn + " out of range")
    dayArray(ndx)
  }
  
  
  private def dayIndex(jdn: Int) = jdn - firstPermittedJuliandayNumber

  def apply(y: Int, m: Int, d: Int): Day = {
    require(y >= 1899 && y <= 2200, "Invalid year: " + y)
    require(d > 0 && d <= DaysInMonthCalc(y, m), s"Invalid day: $d ($y-$m)")
    dayFromJulianDayNumber(
      julianDayNumber(y, m, d)
    )
  }
  
  def daysFromTo(d1: Day, d2: Day): IndexedSeq[Day] = {
    dayArray.slice(dayIndex(d1.ordinal), dayIndex(d2.ordinal + 1))
  }

  def maybeFromExcel(excelDay: Double): Option[Day] = {
    if (excelDay >= earliestPermittedExcelDay) {
      Some(Day.fromOrdinal(excelZeroDay + excelDay.toInt))
    } else {
      None
    }
  }

  def fromExcel(excelDay: Double): Day = {
    maybeFromExcel(excelDay).getOrElse(throw new IllegalArgumentException(s"$excelDay is not a valid Excel day."))
  }

  def fromLocalDate(date: LocalDate): Day = {
    Day(date.getYear, date.getMonthValue, date.getDayOfMonth)
  }

  private val ISOStringFormat = """([\d]{4})\-([\d]{2})\-([\d]{2})""".r

  def fromISO(dayString: String): Option[Day] = dayString.trim match {
    case ISOStringFormat(ParseInt(year), ParseInt(month), ParseInt(day)) => Some(Day(year, month, day))
    case _ => None
  }

  private val ContractIdentifierSuffixFormat = """(\d\d)(\d\d)(\d\d)""".r

  def fromContractIdentifierSuffix(dayStr: String): Either[TopazFail, Day] = dayStr match {
    case ContractIdentifierSuffixFormat(
      ParseInt(day), ParseInt(month), ParseInt(year)) => Right(Day(2000 + year, month, day))
    case _ => TopazFail(s"Couldn't parse contract suffix $dayStr into a day")
  }

  def fromMillisInLocalTime(millis: Long): Day = {
    val cal = JCalendar.getInstance()
    cal.setTimeInMillis(millis)
    Day(cal.get(JCalendar.YEAR), cal.get(JCalendar.MONTH) + 1, cal.get(JCalendar.DAY_OF_MONTH))
  }

  def unapply(s: String): Option[Day] = fromISO(s)

  /**
   * think this algorithm comes from Numerical Recipes
   */
  private def julianDayNumber(year: Int, month: Int, d: Int): Int = {
    var y = year
    var m = month
    if (m > 2) {
      m -= 3 // wash out the leap day
    } else {
      m += 9
      y -= 1
    }
    val c: Int = y / 100
    val ya: Int = y - 100 * c
    ((146097 * c) >> 2) + ((1461 * ya) >> 2) + (153 * m + 2) / 5 + d + 1721119
  }

  /**
   * This should be the only place where a Day instance is actually allocated.
   */
  private def julianDayNumberToYearMonthDay(jdn: Int): (Int, Int, Int) = {
    var j = jdn - 1721119
    var year = ((j << 2) - 1) / 146097
    j = (j << 2) - 1 - 146097 * year
    var d = j >> 2
    j = ((d << 2) + 3) / 1461
    d = (d << 2) + 3 - 1461 * j
    d = (d + 4) >> 2
    var month = (5 * d - 3) / 153
    d = 5 * d - 3 - 153 * month
    val day = (d + 5) / 5
    year = 100 * year + j
    if (month < 10) {
      month += 3
    } else {
      month -= 9
      year += 1
    }
    (year, month, day)
  }

  def toOrdinal(day: Day): Int = julianDayNumber(day.year, day.month, day.dayNumber)

  def fromOrdinal(ordinal: Int): Day = dayFromJulianDayNumber(ordinal)

  override def containing(d: Day): Day = d

  override def toString = "Daily"

}

/**
 * From http://www.tulane.edu/~rscheidt/carter.html
 */
object EasterCalc {
  def easterSunday(year: Int): Day = {
    val b = 225 - 11 * (year % 19)
    var d = ((b - 21) % 30) + 21
    if(d > 48)
      d -= 1
    val e = (year + year / 4 + d + 1) % 7
    val q = d + 7 - e
    if(q < 32)
      Day(year, 3, q)
    else
      Day(year, 4, q - 31)
  }
}

/**
  * This can contain some slightly more exotic date range string representations that we wouldn't
  * want in Day parsing by default.
  */
object DayParser {
  private val SlashOrHyphenStringFormat = """([\d]{1,2})[-/]([\d]{1,2})[-/]([\d]{4})""".r
  private val Excel_DDD_D_MMM_YY_Format = """(\w{3}) ([\d]{1,2}) (\w{3}) ([\d]{2})""".r

  def unapply(str: String): Option[Day] = str match {
    case Day(d) => Some(d)
    case SlashOrHyphenStringFormat(ParseInt(d), ParseInt(m), ParseInt(y)) => Some(Day(y, m, d))
    case Excel_DDD_D_MMM_YY_Format(_, ParseInt(d), MonthOfYear(m), ParseInt(y)) => Some(Day(y + 2000, m.value, d))
    case _ => None
  }
}
