package com.topaz.time

import com.topaz.TopazCodingError
import com.topaz.utils.{GeneralTopazFail, TopazFail}

case class MarketTime(day: Day, timeOfDay: TimeOfDay) extends Ordered[MarketTime] {
  def nextDay: MarketTime = addDays(1)
  def nextWeekday: MarketTime = copy(day = day.nextWeekday)

  def addDays(n: Int): MarketTime = copy(day = day + n)

  def addBusinessDays(calendar: Calendar, n: Int): MarketTime = copy(day = day.addBusinessDays(calendar, n))

  def isUnfixed(observationDay: Day): Boolean = {
    !fixingsShouldExist(observationDay)
  }

  def fixingsShouldExist(observationDay: Day): Boolean = {
    observationDay < day || (observationDay == day && timeOfDay.fixingsShouldExist)
  }

  def pricesCanMove(observationDay: Day): Boolean = {
    observationDay > day || (observationDay == day && timeOfDay.pricesCanMove)
  }
  
  def next(): MarketTime = {
    if (timeOfDay == TimeOfDay.start)
      copy(timeOfDay = TimeOfDay.end)
    else
      copy(day = day + 1, timeOfDay = TimeOfDay.start)
  }

  def compare(that: MarketTime): Int = this.day.compare(that.day) match {
    case 0 => this.timeOfDay.compare(that.timeOfDay)
    case n => n
  } 

  override def toString: String = s"$day ($timeOfDay)"

  def nearestUnfixedDay: Day = if (timeOfDay.fixingsShouldExist) day + 1 else day
}

object MarketTime {
  private val Match = """([\d-]+) \(?([\w ]+)\)?""".r

  def unapply(str: String): Option[MarketTime] = {
    str match {
      case Match(Day(day), TimeOfDay(tod)) => Some(MarketTime(day, tod))
      case _ => None
    }
  }
  
  def parse(str: String): Either[TopazFail, MarketTime] = unapply(str).toRight(
    GeneralTopazFail(s"Invalid string for MarketTime: $str")
  )
}


case class TimeOfDay private(
  pricesCanMove: Boolean, /* Represents the time of day when, although fixings are not necessarily published, the price
                             can no longer change. At this point we have no risk with respect to this price
                          */
  fixingsShouldExist: Boolean
) extends Ordered[TimeOfDay] {

  require(
    !(pricesCanMove && fixingsShouldExist),
    "Cannot have risky prices when fixings exist"
  )

  override def toString: String = this match {
    case TimeOfDay.start => "start of day"
    case TimeOfDay.end => "end of day"
    case _ => super.toString
  }

  def daysBetween(other: TimeOfDay): Int = {
    if (this == other) {
      0
    } else {
      (this, other) match {
        case (TimeOfDay.start, TimeOfDay.end) => 1
        case (TimeOfDay.end, TimeOfDay.start) => -1
        case o => throw TopazCodingError(s"Unexpected times of day: $o")
      }
    }
  }

  def compare(that: TimeOfDay): Int = {
    if (this == that) 
      0 
    else
      (this, that) match {
        case (TimeOfDay.start, TimeOfDay.end) => -1
        case (TimeOfDay.end, TimeOfDay.start) => 1
        case o => throw TopazCodingError(s"Unexpected times of day: $o")
      }
  }
}

object TimeOfDay {
  val start: TimeOfDay = TimeOfDay(pricesCanMove = true, fixingsShouldExist = false)
  val end: TimeOfDay = TimeOfDay(pricesCanMove = false, fixingsShouldExist = true)

  def unapply(str: String): Option[TimeOfDay] = {
    val lowerCase = str.toLowerCase
    if (lowerCase.startsWith("start"))
      Some(start)
    else if (lowerCase.startsWith("end"))
      Some(end)
    else
      None
  }
}
