package com.topaz.time

import com.topaz.TopazCodingError
import com.topaz.utils._

import scala.util.matching.Regex
import scala.language.implicitConversions

/**
 * N.B.
 * When implementing a new DateRange, remember to add the appropriate
 * json formatter to EventStoreJsonProtocol.DateRangeFormat
 */
trait DateRange extends EitherPimpsMixin with RichAnys {
  def firstDay: Day

  def lastDay: Day

  def firstMonth: Month = firstDay.containingMonth

  def lastMonth: Month = lastDay.containingMonth

  def days: IndexedSeq[Day] = Day.daysFromTo(firstDay, lastDay)

  def months: Seq[Month] = firstMonth to lastMonth

  def size: Int = (lastDay - firstDay) + 1

  def contains(other: DateRange): Boolean = other.firstDay >= firstDay && other.lastDay <= lastDay
  def isDisjointWith(other: DateRange): Boolean = other.firstDay > lastDay || other.lastDay < firstDay
  def intersects(other: DateRange): Boolean = !isDisjointWith(other)

  def remainder(inclusive: Day): Option[DateRange] = if (inclusive <= firstDay) {
    Some(this)
  } else if (inclusive > lastDay) {
    None
  } else {
    Some(SimpleDateRange(inclusive, lastDay))
  }

  def upto(inclusive: Day): Option[DateRange] = {
    if (inclusive >= lastDay)
      Some(this)
    else if (inclusive < firstDay)
      None
    else
      Some(DateRange(firstDay, inclusive))
  }

  def normaliseToMonth(f: Day => Boolean): Option[Month] = {
    lazy val bDays = firstMonth.days.filter(f)
    if (firstMonth == lastMonth && bDays.nonEmpty && firstDay <= bDays.head && lastDay >= bDays.last) {
      Some(firstMonth)
    } else {
      None
    }
  }

  def liesOnMonthBoundaries: Boolean = 
    firstDay.isFirstDayOfMonth && lastDay.isLastDayOfMonth

  def intersect(other: DateRange): Option[DateRange] = {
    if(other.firstDay <= this.lastDay && other.lastDay >= this.firstDay)
      Some(DateRange(other.firstDay max this.firstDay, other.lastDay min this.lastDay))
    else
      None
  }

  def difference(rhs: DateRange): IndexedSeq[DateRange] = {
    val period1 = if (firstDay < rhs.firstDay)
      Vector(DateRange(firstDay, (rhs.firstDay - 1).min(lastDay)))
    else
      Vector()
    val period2 = if (lastDay > rhs.lastDay)
      Vector(DateRange((rhs.lastDay + 1).max(firstDay), lastDay))
    else Vector()
    period1 ++ period2
  }

  def periodicityBoundaries(periodicity: DateRangeType): Either[TopazFail, (DateRange, DateRange)] = {
    val l = firstDay.containing(periodicity)
    val r = lastDay.containing(periodicity)
    if (l.firstDay != firstDay || r.lastDay != lastDay)
      TopazFail(s"Can't split $this by $periodicity")
    else
      Right((l, r))
  }


  /**
    * Splits this date range by the given periodicity, so that the union of the split is equivalent
    * to the date range.
    * The last day of this range must lie on a boundary, the first day doesn't need to. This restriction is based
    * on current usage and can be changed if required.
    */
  def splitByPeriodicity(periodicity: DateRangeType): Either[TopazFail, Seq[DateRange]] = {
    val periods = {
      val head +: tail = firstDay.containing(periodicity) to lastDay.containing(periodicity)
      head.intersect(this).get +: tail
    }
    for {
      _ <- ensure(
        periods.last.lastDay == lastDay,
        s"Cannot split $this by $periodicity as last day not on boundary"
      )
    } yield {
      periods
    }
  }

  def splitBy(periodicity: DateRangeType, splitType: SplitType): Seq[DateRange] = {
    val periods = firstDay.containing(periodicity) to lastDay.containing(periodicity)
    splitType match {
      case SplitType.Inner =>
        periods.filter(contains)
      case SplitType.Outer =>
        periods
      case SplitType.Exact =>
        periods.map(_.intersect(this).get)
      case SplitType.LeftExactRightOuter =>
        periods.head.intersect(this).get +: periods.tail
      case SplitType.LeftOuterRightExact =>
        periods.dropRight(1) :+ periods.last.intersect(this).get
    }
  }

  def toPersistentString: String

  def normalised: Option[DateRange] = Some(this)

  def contiguousRanges: Seq[DateRange] = Seq(this)

  def dateRangeType: DateRangeType
  def ordinal: Int

  def next: DateRange = this.+(1)
  def previous: DateRange = this.-(1)
  def +(n: Int): DateRange = dateRangeType.fromOrdinal(ordinal + n)
  def -(n: Int): DateRange = dateRangeType.fromOrdinal(ordinal - n)
  def -(rhs: DateRange): Int = {
    TopazCodingError.require(dateRangeType == rhs.dateRangeType, s"Can't compare date range types $this and $rhs")
    ordinal - rhs.ordinal
  }

  def to(rhs: DateRange): Seq[DateRange] = {
    DateRange.inclusiveRange(this, rhs)
  }
  def toContractIdentifierSuffix: String
  def toContractNameSuffix: String
  def toISO8601String: String

  def isMonth = dateRangeType == Month
  def isDay = dateRangeType == Day
  def isWeek = dateRangeType == Week

  def singleContainingMonth: Option[Month] = months match {
    case Seq(m) => Some(m)
    case _ => None
  }
  def singleContainingQuarter: Option[Quarter] = months match {
    case months if months.map(_.containingQuarter).distinct.size == 1 => Some(months.head.containingQuarter)
    case _ => None
  }
  def singleContainingYear: Option[Year] = months match {
    case months if months.map(_.containingYear).distinct.size == 1 => Some(months.head.containingYear)
    case _ => None
  }
}

object DateRange extends DateRangeSugar {
  private val parseCache = Cache.createStaticCache("DateRange.unapply")
  
  def unapply(str: String): Option[DateRange] = parseCache.memoize(str.trim)(str.trim match {
    case SimpleDateRange(simple) => Some(simple)
    case Day(day) => Some(day)
    case Week(week) => Some(week)
    case Decade(decade) => Some(decade)
    case BOM(bom) => Some(bom)
    case HalfMonth(half) => Some(half)
    case Month(month) => Some(month)
    case Quarter(quarter) => Some(quarter)
    case GasSeason(season) => Some(season)
    case HalfYear(halfYear) => Some(halfYear)
    case Year(year) => Some(year)
    case GasYear(year) => Some(year)
    case GasStorageYear(year) => Some(year)
    case _ => None
  })

  def apply(firstDay: Day, lastDay: Day): DateRange = {
    val sdr = new SimpleDateRange(firstDay, lastDay)
    sdr.normalise.getOrElse(sdr)
  }
  
  def parse(str: String): DateRange = unapply(str).getOrElse(sys.error(s"Not a valid date range: [$str]"))

  def isContiguous(drs: Seq[DateRange]): Boolean = {
    drs.zip(drs.tail).forall{
      case (dr1, dr2) => 
        dr1.lastDay.nextDay == dr2.firstDay
    }
  }
  val samples: Seq[DateRange] = {
    import scala.language.reflectiveCalls
    Seq(
      20 / Jan / 2015,
      Week(2015, 4),
      BOM(1 / Feb / 2015),
      Jan / 2015,
      Quarter(2015, 3),
      Year(2020),
      GasSeason(2010, isSummer = true),
      GasSeason(2010, isSummer = false),
      HalfMonth(2000, 10, halfNumber = 1),
      HalfMonth(2000, 10, halfNumber = 2),
      Decade(2010, 12, 1),
      SimpleDateRange(1 / Jan / 2015, 10 / Feb / 2015)
    )
  }

  def containingPeriod(drs: Seq[DateRange], normalise: Boolean): Option[DateRange] = {
    if (drs.isEmpty)
      None
    else {
      // We do this a lot, so although it's ugly code, it's fast.
      var min: Day = Day.lastPermittedDay
      var max: Day = Day.firstPermittedDay
      drs.foreach {
        dr =>
          val firstDay = dr.firstDay
          if (firstDay < min)
            min = firstDay
          val lastDay = dr.lastDay
          if (lastDay > max)
            max = lastDay
      }
      if (normalise)
        Some(DateRange(min, max))
      else
        Some(SimpleDateRange(min, max))
    }

  }

  def inclusiveRange[T <: DateRange](from:T, to: T): IndexedSeq[T] = {
    if (from.firstDay > to.firstDay) {
      throw new IllegalArgumentException(s"from [$from] > to [$to]")
    }
    TopazCodingError.require(from.dateRangeType == to.dateRangeType, s"Mismatching date ranges $from, $to")
    var range = IndexedSeq(from)
    var t = (from + 1).asInstanceOf[T]
    while (t.firstDay <= to.lastDay){
      range = t +: range
      t = (t + 1).asInstanceOf[T]
    }

    range.reverse
  }

  implicit def ordered[T <: DateRange](lhs: T): Ordered[T] = new Ordered[T]{
    def compare(rhs: T): Int = {

      Integer.compare(lhs.firstDay.ordinal, rhs.firstDay.ordinal) match {
        case 0 => Integer.compare(lhs.lastDay.ordinal, rhs.lastDay.ordinal)
        case o => o
      }
    }
  }

  def pivotCompare(lhs: DateRange, rhs: DateRange): Int = {

    Integer.compare(lhs.firstDay.ordinal, rhs.firstDay.ordinal) match {
      case 0 =>
        /* Reverse rhs/lhs here compared to the more natural ordering. This is so
           2016 appears before Jan 2016 in a pivot report
         */
        Integer.compare(rhs.lastDay.ordinal, lhs.lastDay.ordinal)
      case o => o
    }

  }

  def sizeThenFirstDayCompare(lhs: DateRange, rhs: DateRange): Int = {
    (lhs.size - rhs.size) match {
      case 0 => lhs.firstDay - rhs.firstDay
      case n => n
    }
  }
}

/**
 * All DateRange type companion objects extend this.
 */
trait DateRangeType {
  val name: String = getClass.getSimpleName.takeWhile(_ != '$')
  def fromOrdinal(ordinal: Int): DateRange
  def containing(d: Day): DateRange
  def unapply(s: String): Option[DateRange]
}

object DateRangeType {
  def fromName(name: String): DateRangeType = unapply(name).getOrElse {
    throw new Exception(s"Invalid DateRangeType: $name")
  }
  def unapply(name: String): Option[DateRangeType] = Seq(Year, HalfYear, Quarter, HalfMonth, Month, Decade, Week, Day).find{
    dr => dr.name.equalsIgnoreCase(name.replaceAll(" ", ""))
  }
}

case class SimpleDateRange(firstDay: Day, lastDay: Day) extends DateRange {
  TopazCodingError.require(firstDay <= lastDay, s"Date range out of order first/last $firstDay/$lastDay")
  override def toString: String = toPersistentString

  override def toPersistentString: String = s"$firstDay${SimpleDateRange.Separator}$lastDay"

  def normalise: Option[DateRange] = {
    val possiblePeriods = SimpleDateRange.possibleChronologicalTypes.view.flatMap {
      t =>
        val t1 = t.containing(firstDay)
        val t2 = t.containing(lastDay)
        (t1, t2) match {
          case _ if t1 == t2 => Some(t1)
          case (_: BOM, _) => Some(t1)
          case _ => None
        }
    }
    possiblePeriods.find{
      p => 
        p.firstDay == firstDay && p.lastDay == lastDay
    }
  }

  def dateRangeType: DateRangeType = SimpleDateRange

  def ordinal: Int = throw TopazCodingError(s"Ordinal invalid on $this")

  def toContractIdentifierSuffix: String = throw TopazCodingError(s"toContractIdentifierSuffix not implemented for $this, $getClass")
  def toContractNameSuffix: String = throw TopazCodingError(s"toContractNameSuffix not implemented for $this, $getClass")
  def toISO8601String: String = throw TopazCodingError(s"toISO8601String not implemented for $this, $getClass")
}

object SimpleDateRange extends DateRangeType {
  val Separator: String = "->"
  val RangeRegex: Regex = """(\d{1,2})-(\d{1,2})/(\d{1,2})/(\d{4})""".r

  private val possibleChronologicalTypes: Seq[DateRangeType] = Seq(Day, Week, HalfMonth, Month, BOM, Quarter, GasSeason, Year)

  def unapply(str: String): Option[SimpleDateRange] = {
    val toList = str.split(Separator, 2).toList
    toList match {
      case d1::d2::Nil =>
        for(day1 <- Day.unapply(d1); day2 <- Day.unapply(d2))
          yield new SimpleDateRange(day1, day2)
      case _ => 
        str match {
          case RangeRegex(ParseInt(fromDay), ParseInt(toDay), ParseInt(month), ParseInt(year)) =>
            val to = Day(year, month, toDay)
            val from = Day(year, month, fromDay)
            Some(new SimpleDateRange(from, to))
          case _ =>
            None
        }
    }
  }

  def fromOrdinal(ordinal: Int): DateRange = {
    throw TopazCodingError(s"fromOrdinal is unsupported by SimpleDateRange")
  }

  def containing(d: Day): DateRange = {
    throw TopazCodingError(s"containing is unsupported by SimpleDateRange")
  }

}

