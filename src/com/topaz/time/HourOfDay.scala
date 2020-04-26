package com.topaz.time

import com.topaz.utils.{CollectionUtils, ParseInt}

import scala.collection.immutable
import scala.language.implicitConversions

sealed trait HourOfDay {
  def start: Int
  def sortOrdinal: Int
}

object HourOfDay extends CollectionUtils {
  val all: immutable.IndexedSeq[NormalHour] = (0 to 23).map(NormalHour)
  
  val allSet: Set[HourOfDay] = all.toSet

  val shortDay: immutable.IndexedSeq[HourOfDay] = all.filterNot(_.start == DaylightSavingHour.start)

  val longDay: immutable.IndexedSeq[HourOfDay] = all :+ DaylightSavingHour

  def unapply(hour: Int): Option[HourOfDay] = if (hour >= 0 && hour < 24) Some(all(hour)) else None

  def range(start: Int, end: Int): immutable.IndexedSeq[NormalHour] = all.slice(start, end + 1)
  
  def toString(hours: Set[HourOfDay]): String = {
    CollectionUtils.groupRanges(hours.toSeq.map(_.start).sorted).map {
      case Seq(s, e) => s"$s-$e"
      case Seq(h) => h.toString
    }.mkString(",")
  }
  
  def fromString(str: String): Set[HourOfDay] = {
    if (str.trim.isEmpty) {
      Set()
    } else {
      val RangeRegex = """(\d+)\-(\d+)""".r

      str.split(",").map(_.trim).flatMap {
        case ParseInt(hour) => Seq(NormalHour(hour))
        case RangeRegex(ParseInt(start), ParseInt(end)) => (start to end).map(NormalHour)
      }(collection.breakOut)
    }
  }
}

case class NormalHour(start: Int) extends HourOfDay {
  require(start >= 0 && start <= 23, s"Invalid hour: $start (0-23 are valid hours)")

  def sortOrdinal: Int = start
}

case object DaylightSavingHour extends HourOfDay {
  /**
    * This isn't quite right. In the US they use 2am across all time zones but in Europe
    * the clocks change at 1am GMT, 2am CET and 3am EET so that all zones move
    * together.
    * https://en.wikipedia.org/wiki/Daylight_saving_time
    */
  val start: Int = 2
  
  val name: String = "DaylightSavingHour"
  def sortOrdinal: Int = 24
}
