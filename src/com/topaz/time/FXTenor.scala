package com.topaz.time

import com.topaz.TopazCodingError
import com.topaz.utils.ParseInt

import scala.collection.immutable

trait FXTenor {
  def valueDate(calendar: Calendar, spotDate: Day): Day

  def reportText: String

  override def toString: String = reportText
}

case class FXMonthTenor(months: Int) extends FXTenor {
  def valueDate(calendar: Calendar, spotDate: Day) = {
    FXTenor.valueDateFromValueMonth(
      calendar,
      spotDate.containingMonth + months,
      spotDate
    )
  }

  override def reportText: String = s"${months}M"
}

case class FXYearTenor(years: Int) extends FXTenor {
  def valueDate(calendar: Calendar, spotDate: Day) = {
    FXTenor.valueDateFromValueMonth(
      calendar,
      spotDate.containingMonth + years * 12,
      spotDate
    )
  }

  override def reportText: String = s"${years}Y"
}

case class FXSpecificDayTenor(day: Day) extends FXTenor {
  override def valueDate(calendar: Calendar, spotDate: Day): Day = day

  override def reportText: String = day.toISO8601String
}

object FXTenor {

  def valueDateFromValueMonth(calendar: Calendar, valueMonth: Month, spotDate: Day): Day = {
    val lastBusinessDayInValueMonth = calendar.dayOrPreviousBusinessDay(valueMonth.lastDay)

    if (calendar.isLastBusinessDayInMonth(spotDate) ||
      spotDate.dayNumber > lastBusinessDayInValueMonth.dayNumber)
      lastBusinessDayInValueMonth
    else
      calendar.dayOrNextBusinessDay(valueMonth.day(spotDate.dayNumber))
  }

  val ordering: Ordering[FXTenor] = new Ordering[FXTenor] {
    def compare(x: FXTenor, y: FXTenor): Int = {
      (x, y) match {
        case (FXMonthTenor(n), FXMonthTenor(m)) => n.compareTo(m)
        case (FXYearTenor(n), FXYearTenor(m)) => n.compareTo(m)
        case (FXSpecificDayTenor(d1), FXSpecificDayTenor(d2)) => d1.compareTo(d2)
        case _ =>
          val order: immutable.Seq[Class[_]] = Vector(
            classOf[FXSpecificDayTenor],
            classOf[FXMonthTenor],
            classOf[FXYearTenor],
          )
          order.indexOf(x.getClass).compareTo(order.indexOf(y.getClass))
      }
    }
  }
  private val Pattern = """(\d+)([wmyWMY])""".r

  def fromString(str: String): Option[FXTenor] = {
    def wholeMonthTenor: Option[FXTenor] = {
      str match {
        case Day(day) => Some(FXSpecificDayTenor(day))
        case Pattern(ParseInt(n), typ) =>
          typ.toLowerCase match {
            case "m" => Some(FXMonthTenor(n))
            case "y" => Some(FXYearTenor(n))
            case o => throw TopazCodingError(s"Unexpected type: '$o''")
          }
        case _ => None
      }
    }

    wholeMonthTenor orElse Day.fromISO(str).map(FXSpecificDayTenor)
  }
}
