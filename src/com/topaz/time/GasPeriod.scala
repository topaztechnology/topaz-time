package com.topaz.time

import com.topaz.TopazCodingError
import com.topaz.time.DayOfWeek.Friday
import enumeratum._

import scala.language.reflectiveCalls


sealed abstract class GasPeriodType(override val entryName: String) extends EnumEntry

object GasPeriodType extends Enum[GasPeriodType] {
  val values = findValues
  case object GasDayType extends GasPeriodType("Gas Day")
  case object DayAheadType extends GasPeriodType("Day Ahead")
  case object BalanceOfWeekType extends GasPeriodType("Balance of Week")
  case object SaturdayType extends GasPeriodType("Saturday")
  case object SundayType extends GasPeriodType("Sunday")
  case object GasWeekendType extends GasPeriodType("Gas Weekend")
  case object WorkingDaysNextWeekType extends GasPeriodType("Working Days Next Week")
  case object BalanceOfMonthType extends GasPeriodType("Balance of Month")
  case object MonthType extends GasPeriodType("Month")
  case object QuarterType extends GasPeriodType("Quarter")
  case object GasSeasonType extends GasPeriodType("Gas Season")
  case object GasYearType extends GasPeriodType("Gas Year")
  case object GasStorageYearType extends GasPeriodType("Gas Storage Year")
  case object GasDateRangeType extends GasPeriodType("Gas Date Range")
}

/**
  * All documents mentioned can also be found in Dropbox under Topaz Team/Docs/Natural Gas
  *
  * Gas period definitions are based on the physical market to the extent possible as described in
  * [[https://s3-eu-west-1.amazonaws.com/cjp-rbi-icis-compliance/wp-content/uploads/2019/03/27115859/European-Spot-Gas-Markets-Methodology-25-March-2019.pdf
  * ICIS Spot Gas markets methodology document]]
  *
  * Both NBP and TTF have the same definition of a GasPeriod, including the fact they both
  * use UK bank holidays. NBP refers to 05:00 GMT and TTF refers to 06:00 CET.
  * They also include addition periods over ICIS, so these definitions are used when needed.
  *
  * NBP
  * [[https://www.theice.com/publicdocs/circulars/15258_attach.pdf ICE NBP spec]]
  * TTF
  * [[https://www.theice.com/products/45436633/Dutch-TTF-Gas-Daily-Futures ICE TTF spec]]
  *
  * The rationale is that the primary trading market is the forward market, and ICIS the dominant assessment publisher.
  * The ICE daily gas futures apparently have no open interest, and their definitions are less precise. They have
  * most likely not been updated for some time due to lack of interest. Hence ICIS definitions should be treated
  * as authoritative.
  */
sealed trait GasPeriod {
  def typ: GasPeriodType

  def toPersistentString: String

  def reportText: String = toPersistentString

  def deliveryPeriod(calendar: Calendar): DateRange

  override def toString: String = toPersistentString
}

object GasPeriod {
  private val DA_Pattern = """(?i)(\d{4}-\d{2}-\d{2})-DA""".r
  private val BOW_Pattern = """(?i)(\d{4}-\d{2}-\d{2})-BOW""".r
  private val SAT_Pattern = """(?i)(\d{4}-\d{2}-\d{2})-SAT""".r
  private val SUN_Pattern = """(?i)(\d{4}-\d{2}-\d{2})-SUN""".r
  private val WEND_Pattern = """(?i)(\d{4}-\d{2}-\d{2})-WE""".r
  private val BOM_Pattern = """(?i)(\d{4}-\d{2}-\d{2})-BOM""".r
  private val WDNW_Pattern = """(?i)(\d{4}-\d{2}-\d{2})-WDNW""".r
  private val SUMMER_Pattern = """(?i)(\d{4})-GASSUM(MER)?""".r
  private val WINTER_Pattern = """(?i)(\d{4})-GASWIN(TER)?""".r
  private val GY_Pattern = """(?i)(\d{4})-GY""".r
  private val GSY_Pattern = """(?i)(\d{4})-GSY""".r

  def parse(name: String): GasPeriod =
    unapply(name).getOrElse(throw new Exception(s"Failed to parse '$name' as a GasPeriod"))

  def unapply(name: String): Option[GasPeriod] = name match {
    case SimpleDateRange(dr) => Some(GasDateRange(dr.firstDay, dr.lastDay))
    case Day(d) => Some(GasDay(d))
    case Month(m) => Some(GasMonth(m))
    case Quarter(q) => Some(GasQuarter(q))
    case DA_Pattern(Day(day)) => Some(GasDayAhead(day))
    case BOW_Pattern(Day(day)) => Some(GasBOW(day))
    case SAT_Pattern(Day(w)) => Some(GasSaturday(w))
    case SUN_Pattern(Day(w)) => Some(GasSunday(w))
    case WEND_Pattern(Day(w)) => Some(GasWeekend(w))
    case WDNW_Pattern(Day(w)) => Some(GasWorkingDaysNextWeek(w))
    case BOM_Pattern(Day(day)) => Some(GasBOM(day))
    case SUMMER_Pattern(yearNo, _) => Some(GasSeason(yearNo.toInt, isSummer = true))
    case WINTER_Pattern(yearNo, _) => Some(GasSeason(yearNo.toInt, isSummer = false))
    case GY_Pattern(yearNo) => Some(GasYear(yearNo.toInt))
    case GSY_Pattern(yearNo) => Some(GasStorageYear(yearNo.toInt))
    case _ => None
  }

  val ordering: Ordering[GasPeriod] = (x: GasPeriod, y: GasPeriod) => (x, y) match {
    case (l: GasDateRange, r: GasDateRange) => l.dateRange.compareTo(r.dateRange)
    case (GasDay(l), GasDay(r)) => l.compareTo(r)
    case (GasDayAhead(l), GasDayAhead(r)) => l.compareTo(r)
    case (GasBOW(l), GasBOW(r)) => l.compareTo(r)
    case (GasSaturday(l), GasSaturday(r)) => l.compareTo(r)
    case (GasSunday(l), GasSunday(r)) => l.compareTo(r)
    case (GasWeekend(l), GasWeekend(r)) => l.compareTo(r)
    case (GasWorkingDaysNextWeek(l), GasWorkingDaysNextWeek(r)) => l.compareTo(r)
    case (GasBOM(l), GasBOM(r)) => l.compareTo(r)
    case (GasMonth(l), GasMonth(r)) => l.compareTo(r)
    case (GasQuarter(l), GasQuarter(r)) => l.compareTo(r)
    case (l: GasSeason, r: GasSeason) => l.compareTo(r)
    case (l: GasYear, r: GasYear) => l.yearNumber.compareTo(r.yearNumber)
    case (l: GasStorageYear, r: GasStorageYear) => l.yearNumber.compareTo(r.yearNumber)
    case (l, r) =>
      val order = Vector(
        classOf[GasDateRange],
        classOf[GasYear],
        classOf[GasStorageYear],
        classOf[GasSeason],
        classOf[GasQuarter],
        classOf[GasMonth],
        classOf[GasDay],
        classOf[GasDayAhead],
        classOf[GasBOW],
        classOf[GasSaturday],
        classOf[GasSunday],
        classOf[GasWeekend],
        classOf[GasWorkingDaysNextWeek],
        classOf[GasBOM],
      )
      order.indexOf(l.getClass).compareTo(order.indexOf(r.getClass))
  }
}

/**
  * Added to allow proprietary arbitrary delivery periods
  */
case class GasDateRange(firstDay: Day, lastDay: Day) extends GasPeriod {
  val dateRange = SimpleDateRange(firstDay, lastDay)

  def typ: GasPeriodType = GasPeriodType.GasDateRangeType

  def deliveryPeriod(calendar: Calendar): DateRange = dateRange

  def toPersistentString: String = dateRange.toPersistentString
}

object GasDateRange {
  def apply(dr: DateRange): GasPeriod = dr match {
    case m: Month => GasMonth(m)
    case q: Quarter => GasQuarter(q)
    case s: GasSeason => s
    case y: GasYear => y
    case y: GasStorageYear => y
    case o => GasDateRange(o.firstDay, o.lastDay)
  }
}

/**
  * ICIS definition
  *
  * All periods quoted are based on the standard European
  * definition of the Gas Day. This runs from 06:00:00 until
  * 05:59:59 local time on the following day. The exceptions
  * are as follows:
  *
  * 1. Turkey 08:00:00 to 07:59:59 local time.
  * 2. Britain, 05:00:00 to 04:59:59 local time.
  * 3. Ukraine, 05:00:00 to 04:59:00 local time.
  *
  * All prices are for flat gas, no swing and 100% take-or-pay
  * to be delivered for the duration of the contract to the hub.
  */
case class GasDay(day: Day) extends GasPeriod {
  def typ: GasPeriodType = GasPeriodType.GasDayType

  def deliveryPeriod(calendar: Calendar): DateRange = day

  def toPersistentString: String = day.toISO8601String
}


/**
  * ICIS definition
  *
  * Day-ahead: This refers to the next working Gas Day in
  * England following the date of the report. In a report
  * published on Friday, Day-ahead would normally apply to
  * the following Monday, provided that Monday is not a
  * public holiday. In this case, Day-ahead would refer to the
  * next working day, that is Tuesday.
  */
case class GasDayAhead(day: Day) extends GasPeriod {
  def typ: GasPeriodType = GasPeriodType.DayAheadType

  def deliveryPeriod(calendar: Calendar): DateRange = {
    val workdayCalendar = calendar && WeekdayCalendar
    day.addBusinessDays(workdayCalendar, 1)
  }

  def toPersistentString: String = s"${day.toISO8601String}-DA"
}

/**
  * ICE definition
  *
  * The Balance of Week contract (BOW) is a strip that spans four, three or
  * two individual and consecutive gas days from Tuesday 5:00 (GMT/BST)
  * through to Saturday 05:00 (GMT/BST), Wednesday 5:00 (GMT/BST)
  * through to Saturday 05:00 (CET) or Thursday 5:00 (GMT/BST) through
  * to Saturday 05:00 (GMT/BST) respectively. UK Bank Holidays on
  * Tuesday and/or Friday are not included in the BOW contract.
  */
case class GasBOW(fromDay: Day) extends GasPeriod {
  def typ: GasPeriodType = GasPeriodType.BalanceOfWeekType

  def deliveryPeriod(calendar: Calendar): DateRange = {
    val workdayCalendar = calendar && WeekdayCalendar

    val start = fromDay.addBusinessDays(workdayCalendar, 1)

    // If we ask for BOW with fromDay on a Friday (or a Thursday and Friday is a holiday) we will move
    // into a new week.
    val newStart = if (start.containingWeek > fromDay.containingWeek) {
      // BOW runs from a Tuesday to a Friday (as defined in the class comment)
      start.containingWeek.tuesday.thisOrNextBusinessDay(workdayCalendar)
    } else {
      start
    }
    val end = newStart.thisOrNext(Friday).thisOrPreviousBusinessDay(workdayCalendar)
    DateRange(newStart, end)
  }

  def toPersistentString = s"${fromDay.toISO8601String}-BOW"
}

/**
  * ICE definition
  *
  * The Saturday contract (Saturday) is a strip of a single gas day from
  * Saturday 05:00 (GMT/BST) through to Sunday 05:00 (GMT/BST).
  */
case class GasSaturday(day: Day) extends GasPeriod {
  def typ: GasPeriodType = GasPeriodType.SaturdayType

  def deliveryPeriod(calendar: Calendar): DateRange = day.containingWeek.saturday

  def toPersistentString: String = s"${day.toPersistentString}-SAT"
}

/**
  * ICE definition
  *
  * The Sunday contract (Sunday) is a strip of a single gas day from Sunday
  * 05:00 (GMT/BST) through to Monday 05:00 (GMT/BST).
  */
case class GasSunday(day: Day) extends GasPeriod {
  def typ: GasPeriodType = GasPeriodType.SundayType

  def deliveryPeriod(calendar: Calendar): DateRange = day.containingWeek.sunday

  def toPersistentString: String = s"${day.toPersistentString}-SUN"
}

/**
  * ICIS definition
  *
  * Weekend: This covers the first Saturday and Sunday
  * following the date of the report. Other contiguous nonworking
  * days – typically Bank Holiday Mondays and
  * Good Friday – will be added to the contract to create a
  * three- or four-day delivery period.
  * When Christmas Day and Boxing Day (25-26
  * December) and New Year’s Day (1 January) fall midweek
  * these shall be considered additional Weekend
  * delivery periods. For example, if 25-26 December fall on
  * a Tuesday and Wednesday, the Weekend contract
  * quoted on the Monday of the same week would refer to
  * Tuesday and Wednesday. The weekend after 25-26
  * December would be assessed as a contract for the first
  * time on 27 December.
  */
case class GasWeekend(day: Day) extends GasPeriod {
  def typ: GasPeriodType = GasPeriodType.GasWeekendType

  def deliveryPeriod(calendar: Calendar): DateRange = {
    val workdayCalendar = calendar && WeekdayCalendar

    // We may already be in a weekend so move to next working day
    val nextWorkingDay = day.thisOrNextBusinessDay(workdayCalendar)
    val start = nextWorkingDay.thisOrNextNonBusinessDay(workdayCalendar)
    val end = start.thisOrNextBusinessDay(workdayCalendar) - 1

    DateRange(start, end)
  }

  def toPersistentString = s"${day.toPersistentString}-WE"
}

/**
  * ICIS definition
  *
  * WDNW: The contract covers the Working Days Next
  * Week period to be delivered on every working day in the
  * week following the date of the report. Typically this
  * covers the five days following the next Weekend
  * contract. In the case of a three-day weekend covering
  * Saturday to Monday, Monday would be excluded from
  * the WDNW contract, which would instead cover
  * Tuesday-Friday.
  * Similarly, in the case where Christmas Day and Boxing
  * Day (25-26 December) fall on a Tuesday and
  * Wednesday, the WDNW contract assessed on the
  * previous Friday (21 December) will refer only to the
  * following Monday (24 December). The WDNW contract
  * assessed on Monday (24 December) will refer to
  * Thursday and Friday (27-28 December) only.
  */
case class GasWorkingDaysNextWeek(day: Day) extends GasPeriod {
  def typ: GasPeriodType = GasPeriodType.WorkingDaysNextWeekType

  def deliveryPeriod(calendar: Calendar): DateRange = {
    val workdayCalendar = calendar && WeekdayCalendar

    // We may already be in a weekend so move to next working day
    val nextWorkingDay = day.thisOrNextBusinessDay(workdayCalendar)
    val nextNonWorkingDay = nextWorkingDay.thisOrNextNonBusinessDay(workdayCalendar)
    val start = nextNonWorkingDay.thisOrNextBusinessDay(workdayCalendar)
    val end = start.thisOrNextNonBusinessDay(workdayCalendar) - 1

    DateRange(start, end)
  }

  def toPersistentString = s"${day.toPersistentString}-WDNW"
}

/**
  * ICIS definition
  *
  * BOM: Balance-of-month is for delivery on each of the
  * remaining days of the current month, excluding the next
  * Day-ahead or Weekend contract, whichever is the
  * sooner.
  * For example, on a Thursday, BOM would normally apply
  * from the following Saturday to the end of the month
  * (remaining days of the month minus Day-ahead). On a
  * Friday, BOM would normally apply from the following
  * Monday to the end of the month (remaining days of the
  * month minus Weekend).
  * Should the next Day-ahead or Weekend contract cover
  * the last day or days of the current month, the delivery
  * period to which BOM applies will switch to the next
  * month. Should the next Day-ahead or Weekend contract
  * cover the first day or days of the next month, BOM will
  * apply to the subsequent days of that month.
  */
case class GasBOM(fromDay: Day) extends GasPeriod {
  def typ: GasPeriodType = GasPeriodType.BalanceOfMonthType

  def deliveryPeriod(calendar: Calendar): DateRange = {
    val dayAhead = GasDayAhead(fromDay).deliveryPeriod(calendar)
    val weekend = GasWeekend(fromDay).deliveryPeriod(calendar)

    val start = if (dayAhead.firstDay < weekend.firstDay)
      dayAhead.lastDay + 1
    else
      weekend.lastDay + 1

    DateRange(start, start.containingMonth.lastDay)
  }

  def toPersistentString: String = s"${fromDay.toISO8601String}-BOM"
}

/**
  * ICIS definition
  *
  * Months: Monthly contracts refer to the Gas Days of the
  * standard calendar months.
  */
case class GasMonth(month: Month) extends GasPeriod {
  def typ: GasPeriodType = GasPeriodType.MonthType

  def deliveryPeriod(calendar: Calendar): DateRange = month

  def toPersistentString: String = month.toISO8601String
}

/**
  * ICIS definition
  *
  * Quarters: A Quarter refers to the Gas Days of the three
  * month periods beginning on 1 January (Q1), 1 April
  * (Q2), 1 July (Q3) and 1 October (Q4).
  */
case class GasQuarter(quarter: Quarter) extends GasPeriod {
  def typ: GasPeriodType = GasPeriodType.QuarterType

  def deliveryPeriod(calendar: Calendar): DateRange = quarter

  def toPersistentString: String = quarter.toPersistentString
}

/**
  * ICIS definition
  *
  * Seasons: A Season refers to the Gas Days of the six
  * month period running from either 1 April - 30 September
  * (Summer) or from 1 October of one year to 31 March of
  * the following year (Winter). The title of the Winter always
  * refers to the year in which the contract commences. For
  * EUROPEAN SPOT GAS MARKET METHODOLOGY 12
  * example, Winter 2013 starts on 1 October 2013.
  */
case class GasSeason(year: Int, isSummer: Boolean) extends DateRange with GasPeriod {
  require(year >= 1900 && year < 3000, s"Invalid year: $year")

  def firstDay: Day = if (isSummer) Day(year, 4, 1) else Day(year, 10, 1)

  def lastDay: Day = if (isSummer) Day(year, 9, 30) else Day(year + 1, 3, 31)

  def typ: GasPeriodType = GasPeriodType.GasSeasonType

  def seasonName: String = if (isSummer) "GasSummer" else "GasWinter"

  override def toString: String = toPersistentString

  override def toPersistentString: String = f"$year%4d-$seasonName"

  def deliveryPeriod(calendar: Calendar): DateRange = this

  def dateRangeType: DateRangeType = GasSeason

  override val ordinal: Int = year << 1 + (if (isSummer) 1 else 0)

  def toContractIdentifierSuffix: String = throw TopazCodingError(s"toContractIdentifierSuffix not implemented for $this, $getClass")

  def toContractNameSuffix: String = throw TopazCodingError(s"toContractNameSuffix not implemented for $this, $getClass")

  def toISO8601String: String = throw TopazCodingError(s"toISO8601String not implemented for $this, $getClass")
}

object GasSeason extends DateRangeType {
  def apply(d: Day): GasSeason = {
    Seq(
      GasSeason(d.year - 1, isSummer = false),
      GasSeason(d.year, isSummer = true),
      GasSeason(d.year, isSummer = false)
    ).view.find(_.contains(d)).get
  }

  def unapply(str: String): Option[GasSeason] = str match {
    case GasPeriod(gs: GasSeason) => Some(gs)
    case _ => None
  }

  override def containing(d: Day): GasSeason = apply(d)

  def fromOrdinal(ordinal: Int): DateRange = {
    val summer = ordinal % 2 == 1
    val year = ordinal >> 1
    GasSeason(year, summer)
  }
}

/**
  * ICIS definition
  *
  * Gas Year: A Gas Year refers to the Gas Days of the 12-
  * month period from 1 October of a particular calendar
  * year and ending on 30 September of the following
  * calendar year. The title of the Gas Year always refers to
  * the year in which the contract commences. For example,
  * Gas Year 2013 starts on 1 October 2013
  */
case class GasYear(yearNumber: Int) extends DateRange with GasPeriod {
  require(Year.isValidYearNumber(yearNumber), s"Invalid year: $yearNumber")

  def firstDay: Day = Day(yearNumber, 10, 1)

  def lastDay: Day = Day(yearNumber + 1, 9, 30)

  def typ: GasPeriodType = GasPeriodType.GasYearType

  def toPersistentString: String = s"$yearNumber-GY"

  def deliveryPeriod(calendar: Calendar): DateRange = this

  def dateRangeType: GasStorageYear.type = GasStorageYear

  override val ordinal: Int = yearNumber

  override def next: GasYear = super.next.asInstanceOf[GasYear]

  override def previous: GasYear = super.next.asInstanceOf[GasYear]

  def to(rhs: GasYear): Seq[GasYear] = DateRange.inclusiveRange[GasYear](this, rhs)

  def toContractIdentifierSuffix: String = throw TopazCodingError(s"toContractIdentifierSuffix not implemented for $this, $getClass")

  def toContractNameSuffix: String = throw TopazCodingError(s"toContractNameSuffix not implemented for $this, $getClass")

  def toISO8601String: String = toPersistentString
  override def +(n: Int): GasYear = super.+(n).asInstanceOf[GasYear]
  override def -(n: Int): GasYear = super.-(n).asInstanceOf[GasYear]
}

object GasYear extends DateRangeType with DateRangeSugar {
  def unapply(str: String): Option[GasYear] = str match {
    case GasPeriod(gy: GasYear) => Some(gy)
    case _ => None
  }

  def containing(day: Day): GasYear = {
    if (day >= 1 / Oct / day.year)
      GasYear(day.year)
    else
      GasYear(day.year - 1)
  }

  override def fromOrdinal(ordinal: Int): GasYear = GasYear(ordinal)
}

/**
  * Gas storage year is the usual contract period for gas storage facilities so added for convenience
  */
case class GasStorageYear(yearNumber: Int) extends DateRange with GasPeriod {
  require(Year.isValidYearNumber(yearNumber), s"Invalid year: $yearNumber")

  def firstDay: Day = Day(yearNumber, 4, 1)

  def lastDay: Day = Day(yearNumber + 1, 3, 31)

  def typ: GasPeriodType = GasPeriodType.GasStorageYearType

  def toPersistentString: String = s"$yearNumber-GSY"

  def deliveryPeriod(calendar: Calendar): DateRange = this

  def dateRangeType: GasStorageYear.type = GasStorageYear

  override val ordinal: Int = yearNumber

  override def next: GasStorageYear = super.next.asInstanceOf[GasStorageYear]

  override def previous: GasStorageYear = super.next.asInstanceOf[GasStorageYear]

  def to(rhs: GasStorageYear): Seq[GasStorageYear] = DateRange.inclusiveRange[GasStorageYear](this, rhs)

  def toContractIdentifierSuffix: String = throw TopazCodingError(s"toContractIdentifierSuffix not implemented for $this, $getClass")

  def toContractNameSuffix: String = throw TopazCodingError(s"toContractNameSuffix not implemented for $this, $getClass")

  def toISO8601String: String = toPersistentString
}

object GasStorageYear extends DateRangeType with DateRangeSugar {
  def unapply(str: String): Option[GasStorageYear] = str match {
    case GasPeriod(gy: GasStorageYear) => Some(gy)
    case _ => None
  }

  def containing(day: Day): GasStorageYear = {
    if (day >= 1 / Apr / day.year)
      GasStorageYear(day.year)
    else
      GasStorageYear(day.year - 1)
  }

  override def fromOrdinal(ordinal: Int): GasStorageYear = GasStorageYear(ordinal)
}
