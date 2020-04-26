package com.topaz.time

import com.topaz.time.Tenor.Years
import enumeratum.values.{IntEnum, IntEnumEntry}

import scala.collection.immutable

sealed abstract class RiskPeriodType(val value: Int) extends IntEnumEntry
object RiskPeriodType extends IntEnum[RiskPeriodType ]{
  val values: immutable.IndexedSeq[RiskPeriodType] = findValues
  case object InitialStorageRiskPeriodType extends RiskPeriodType(0)
  case object SpotRiskPeriodType extends RiskPeriodType(1)
  case object GasPeriodRiskPeriodType extends RiskPeriodType(2)
  case object DateRangeRiskPeriodType extends RiskPeriodType(3)
  case object SpreadRiskPeriodType extends RiskPeriodType(4)
  case object IRSwapRiskPeriodType extends RiskPeriodType(5)
  case object FRARiskPeriodType extends RiskPeriodType(6)
  case object TerminalStorageRiskPeriodType extends RiskPeriodType(7)
  case object ExpiredRiskPeriodType extends RiskPeriodType(8)
  case object RemainderRiskPeriodType extends RiskPeriodType(9)
  case object CurrentStorageRiskPeriodType extends RiskPeriodType(10)
  case object TenorRiskPeriodType extends RiskPeriodType(11)
}

import com.topaz.time.RiskPeriodType._
sealed trait RiskPeriod extends Comparable[RiskPeriod] {
  def periodType: RiskPeriodType
  def reportText: String

  protected def compareDifferentTypes(o: RiskPeriod): Int = {
    // sort spreads after everything else #138032061
    periodType.value - o.periodType.value
  }
}

object RiskPeriod {
  def apply(dateRange: DateRange): DateRangeRiskPeriod = DateRangeRiskPeriod(dateRange)
  def apply(month1: Month, month2: Month): SpreadRiskPeriod = SpreadRiskPeriod(month1, month2)
}

sealed trait SingleDateRangeRiskPeriod extends RiskPeriod {
  def dateRange: DateRange
}

case class DateRangeRiskPeriod(dateRange: DateRange) extends SingleDateRangeRiskPeriod {
  def reportText: String = dateRange.toString
  override def toString: String = reportText

  override def compareTo(o: RiskPeriod): Int = o match {
    case DateRangeRiskPeriod(other) => DateRange.pivotCompare(dateRange, other)
    case s: SingleDateRangeRiskPeriod => -s.compareTo(this)
    case _ => super.compareDifferentTypes(o)
  }

  def periodType: RiskPeriodType = DateRangeRiskPeriodType
}

case class InitialStorageRiskPeriod(dateRange: DateRange) extends SingleDateRangeRiskPeriod {

  def reportText: String = s"Initial ${dateRange.toString}"

  def periodType: RiskPeriodType = InitialStorageRiskPeriodType

  def compareTo(o: RiskPeriod): Int = o match {
    case InitialStorageRiskPeriod(other) => DateRange.pivotCompare(dateRange, other)
    case s: SingleDateRangeRiskPeriod =>
      if (dateRange.firstDay == s.dateRange.firstDay)
        -1
      else
        DateRange.pivotCompare(dateRange, s.dateRange)
    case _ => super.compareDifferentTypes(o)
  }
}

case class CurrentStorageRiskPeriod(day: Day) extends SingleDateRangeRiskPeriod {
  def dateRange: DateRange = day

  def reportText: String = s"Current ${dateRange.toString}"

  def periodType: RiskPeriodType = CurrentStorageRiskPeriodType

  def compareTo(o: RiskPeriod): Int = o match {
    case InitialStorageRiskPeriod(other) => DateRange.pivotCompare(dateRange, other)
    case TerminalStorageRiskPeriod(_) => -1
    case s: SingleDateRangeRiskPeriod =>
      if (dateRange.firstDay == s.dateRange.firstDay)
        -1
      else
        day.compareTo(s.dateRange.lastDay) // We want current(day in month) to appear before the month
    case _ => super.compareDifferentTypes(o)
  }
}

case class TerminalStorageRiskPeriod(dateRange: DateRange) extends SingleDateRangeRiskPeriod {

  def reportText: String = s"Terminal ${dateRange.toString}"

  def periodType: RiskPeriodType = TerminalStorageRiskPeriodType

  def compareTo(o: RiskPeriod): Int = o match {
    case TerminalStorageRiskPeriod(other) => DateRange.pivotCompare(dateRange, other)
    case s: SingleDateRangeRiskPeriod =>
      if (dateRange.firstDay == s.dateRange.firstDay)
        +1
      else
        DateRange.pivotCompare(dateRange, s.dateRange)
    case _ => super.compareDifferentTypes(o)
  }
}

case class SpreadRiskPeriod(period1: DateRange, period2: DateRange) extends RiskPeriod {
  def reportText: String = s"$period1/$period2"
  override def toString: String = reportText

  override def compareTo(o: RiskPeriod): Int = o match {
    case SpreadRiskPeriod(otherPeriod1, otherPeriod2) =>
      period1.compareTo(otherPeriod1) match {
        case 0 => period2.compareTo(otherPeriod2)
        case n => n
      }
    case _ => super.compareDifferentTypes(o)
  }
  def periodType: RiskPeriodType = SpreadRiskPeriodType
}

case class IRSwapRiskPeriod(maturity: Years) extends RiskPeriod {
  def reportText: String = s"${maturity.reportText}"

  def compareTo(o: RiskPeriod): Int = o match {
    case IRSwapRiskPeriod(otherMaturity)  => maturity.numMonths - otherMaturity.numMonths
    case _ => super.compareDifferentTypes(o)
  }
  def periodType: RiskPeriodType = IRSwapRiskPeriodType
}

case class FRARiskPeriod(startPeriod: WholeMonthTenor, tenor: WholeMonthTenor) extends RiskPeriod {
  def reportText = s"${startPeriod.reportText} x ${tenor.reportText}"

  def compareTo(o: RiskPeriod) = o match {
    case FRARiskPeriod(otherTenorStart, otherTenor) =>
      startPeriod.numMonths - otherTenorStart.numMonths match {
        case 0 => tenor.numMonths - otherTenor.numMonths
        case n => n
      }
    case _ => super.compareDifferentTypes(o)
  }
  def periodType: RiskPeriodType = FRARiskPeriodType
}

case class GasPeriodRiskPeriod(gasPeriod: GasPeriod) extends RiskPeriod {
  def reportText: String = gasPeriod.reportText

  def compareTo(o: RiskPeriod): Int = o match {
    case GasPeriodRiskPeriod(otherGasPeriod) => GasPeriod.ordering.compare(gasPeriod, otherGasPeriod)
    case _ => super.compareDifferentTypes(o)
  }

  def periodType: RiskPeriodType = GasPeriodRiskPeriodType
}

case class TenorRiskPeriod(tenor: FXTenor) extends RiskPeriod {
  def reportText: String = tenor.reportText

  def compareTo(o: RiskPeriod): Int = o match {
    case TenorRiskPeriod(other) => FXTenor.ordering.compare(tenor, other)
    case _ => super.compareDifferentTypes(o)
  }

  def periodType: RiskPeriodType = TenorRiskPeriodType
}

case object SpotRisk extends RiskPeriod {
  def reportText: String = "Spot"

  def compareTo(o: RiskPeriod): Int = o match {
    case SpotRisk => 0
    case _ => super.compareDifferentTypes(o)
  }
  def periodType: RiskPeriodType = SpotRiskPeriodType
}

case object ExpiredRiskPeriod extends RiskPeriod {
  def reportText: String = "Expired"

  def compareTo(o: RiskPeriod): Int = o match {
    case ExpiredRiskPeriod => 0
    case _ => super.compareDifferentTypes(o)
  }
  def periodType: RiskPeriodType = ExpiredRiskPeriodType
}

case class RemainderRiskPeriod(from: DateRange) extends RiskPeriod {
  def reportText: String = s"$from ->"

  def compareTo(o: RiskPeriod): Int = o match {
    case RemainderRiskPeriod(other) => from.compareTo(other)
    case _ => super.compareDifferentTypes(o)
  }
  def periodType: RiskPeriodType = RemainderRiskPeriodType
}

object RemainderRiskPeriod {
  def fromDay(lastDayForFineGrainedPositions: Day): RemainderRiskPeriod = {
    val nextDay = lastDayForFineGrainedPositions.nextDay
    val periodLabel = if (nextDay.month == 1 && nextDay.dayNumber == 1)
      nextDay.containingYear
    else if (nextDay.dayNumber == 1)
      nextDay.containingMonth
    else
      nextDay
    RemainderRiskPeriod(periodLabel)
  }
}
