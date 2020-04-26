package com.topaz.time

import com.topaz.TopazCodingError
import com.topaz.utils.TopazFail

import scala.math.Ordering

// Marker trait for things that can be vol period keys
trait VolPeriod {
  def isOutright: Boolean = isInstanceOf[DateRange]
  def asOutright: Either[TopazFail, DateRange] = {
    if (isOutright)
      Right(asInstanceOf[DateRange])
    else
      TopazFail(s"VolPeriod $this is not a DateRange")
  }

  def asMonth: Either[TopazFail, Month] = {
    this match {
      case m: Month => Right(m)
      case _ => TopazFail("Expected month, was $this")
    }
  }

  def asSpreadPair: Either[TopazFail, UnorderedMonthPair] = {
    this match {
      case pair: UnorderedMonthPair => Right(pair)
      case other => TopazFail(s"VolPeriod $this is not an UnorderedMonthPair")
    }
  }
}

trait SpreadVolPeriod extends VolPeriod {
  def spreadWidth: Int
}

object VolPeriod {
  def unapply(s: String): Option[VolPeriod] = OutrightVolPeriod.unapply(s) orElse SpreadVolPeriod.unapply(s)

  val ord: Ordering[VolPeriod] = {
    case (x: DateRange, y: DateRange) => x.compare(y)
    case (x: UnorderedMonthPair, y: UnorderedMonthPair) => UnorderedMonthPair.ord.compare(x, y)
    case (_, other) => throw TopazCodingError(s"Unexpected vol period $other")
  }
}

object OutrightVolPeriod {
  def unapply(s: String): Option[VolPeriod] =
    Day.unapply(s) orElse
    Week.unapply(s) orElse
    HalfMonth.unapply(s) orElse
    Month.unapply(s)
}

object SpreadVolPeriod {
  def unapply(s: String): Option[SpreadVolPeriod] =
    UnorderedDayPair.unapply(s) orElse
    UnorderedMonthPair.unapply(s)
}
