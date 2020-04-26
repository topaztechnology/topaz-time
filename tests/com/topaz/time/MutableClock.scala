package com.topaz.time

import java.util.concurrent.atomic.AtomicLong

import scala.concurrent.duration.FiniteDuration

class MutableClock(timeMillis: Long = 0) extends Clock {
  private val time = new AtomicLong(timeMillis)

  override def milliTime(): Long = time.longValue()

  def +=(duration: FiniteDuration): Unit = {
    time.addAndGet(duration.toMillis)
  }

  override def toString: String = time.toString
}

class IncrementingClock(timeMillis: Long = 0) extends Clock {
  private val lastTime = new AtomicLong(timeMillis)

  def milliTime(): Long = {
    lastTime.getAndIncrement()
  }
}
