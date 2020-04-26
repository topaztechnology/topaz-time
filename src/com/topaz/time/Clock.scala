package com.topaz.time

import java.text.SimpleDateFormat
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset, ZonedDateTime}
import java.util.{Date, TimeZone}

import akka.util.Timeout
import com.topaz.time.Clock.FrozenTimeClock

import scala.concurrent.duration.{Deadline, Duration, FiniteDuration}

trait Clock {
  def milliTime(): Long
  
  def now(): Timestamp = Timestamp(milliTime())
  
  def frozen(): FrozenTimeClock = new FrozenTimeClock(milliTime())

  def today(): Day = Day.fromMillisInLocalTime(now().milliTime)
  
  def format(pattern: String): String = {
    val sdf = new SimpleDateFormat(pattern)
    sdf.format(Date.from(Instant.ofEpochMilli(milliTime())))
  }
  
  def deadline(): Deadline = Deadline(Duration(milliTime(), "ms"))

  override def toString: String = {
    format("yyyy-MM-dd HH:mm:ss.SSS")
  }
}

case class Timestamp(milliTime: Long) extends Ordered[Timestamp] {
  def olderThan(duration: FiniteDuration)(implicit clock: Clock): Boolean = {
    clock.milliTime - milliTime > duration.toMillis
  }
  
  def hasTimedOut(timeout: FiniteDuration)(implicit clock: Clock): Boolean = {
    (clock.milliTime - milliTime) >= timeout.toMillis
  }

  def hasTimedOut(timeout: Timeout)(implicit clock: Clock): Boolean = hasTimedOut(timeout.duration)

  def +(duration: FiniteDuration): Timestamp = copy(milliTime + duration.toMillis)

  def compare(that: Timestamp): Int = java.lang.Long.compare(milliTime, that.milliTime)

  def format(pattern: String, timeZone: String): String = {
    val sdf = new SimpleDateFormat(pattern)
    sdf.setTimeZone(TimeZone.getTimeZone(timeZone))
    sdf.format(Date.from(Instant.ofEpochMilli(milliTime)))
  }

  def excelFormat: String = {
    format("dd MMM yyyy HH:mm:ss", "UTC")
  }

  def toISOString: String = { // ISO 8601 format
    DateTimeFormatter.ISO_INSTANT.format(Instant.ofEpochMilli(milliTime))
  }

  override def toString: String = {
    format("yyyy-MM-dd HH:mm:ss.SSS", "UTC")
  }

  def toSecString: String = {
    format("yyyy-MM-dd HH:mm:ss", "UTC")
  }

  def roundToSeconds: Timestamp = {
    Timestamp(((milliTime + 500) / 1000) * 1000)
  }
}

object Timestamp {
  def parseISO(string: String): Timestamp = {
    val format = DateTimeFormatter.ISO_INSTANT.withZone(ZoneOffset.UTC)
    Timestamp(ZonedDateTime.parse(string, format).toInstant.toEpochMilli)
  }
}

/**
 * We generally aren't sensitive to wall clock time, but some places, like populating
 * Day choosers in a GUI, will be. This allows us to run the system with a specific
 * wall clock time.
 */
object Clock {

  object WallClock extends Clock {
    def milliTime(): Long = System.currentTimeMillis()
  }

  class FrozenTimeClock(frozenMilliTime: Long) extends Clock {
    def milliTime(): Long = frozenMilliTime
  }
}
