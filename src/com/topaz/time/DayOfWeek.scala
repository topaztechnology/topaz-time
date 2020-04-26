package com.topaz.time

import com.topaz.utils.{NamedIntEnum, NamedIntEnumEntry}

import scala.collection.immutable

sealed abstract class DayOfWeek(val value: Int, val name: String) extends NamedIntEnumEntry {
  override def toString: String = name

  def numDaysUntilNext(other: DayOfWeek): Int = {
    if(value < other.value)
      other.value - value
    else if(value > other.value)
      7 - (value - other.value)
    else
      7
  }
  
  def next(): DayOfWeek = DayOfWeek.withValue((value + 1) % 7)

  def isWeekday: Boolean = !isWeekend

  def isWeekend: Boolean = value >= 5
}

object DayOfWeek extends NamedIntEnum[DayOfWeek] {
  val values: immutable.IndexedSeq[DayOfWeek] = findValues

  val weekDays: immutable.IndexedSeq[DayOfWeek] = findValues.filterNot(_.isWeekend)
  val weekendDays: immutable.IndexedSeq[DayOfWeek] = findValues.filter(_.isWeekend)

  def unapply(arg: Int): Option[DayOfWeek] = {
    if (arg >= 0 && arg < 7)
      Some(values(arg))
    else
      None
  }

  case object Monday extends DayOfWeek(0, "Monday")
  case object Tuesday extends DayOfWeek(1, "Tuesday")
  case object Wednesday extends DayOfWeek(2, "Wednesday")
  case object Thursday extends DayOfWeek(3, "Thursday")
  case object Friday extends DayOfWeek(4, "Friday")
  case object Saturday extends DayOfWeek(5, "Saturday")
  case object Sunday extends DayOfWeek(6, "Sunday")
}
