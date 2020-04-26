package com.topaz.time

import com.topaz.time.RateLimiter.RateLimitExceeded

import scala.concurrent.Future
import scala.concurrent.duration.{Deadline, FiniteDuration}

/**
  * Rate Limiter from Reactive Design Patterns by Roland Kuhn
  * 
  * See the example project here
  * https://github.com/calvinlfer/rate-limiting-and-gating-akka/
  *
  * @param requests is the number of requests allowed in the specified period
  */
case class RateLimiter(requests: Int, period: FiniteDuration)(implicit clock: Clock) {
  require(requests <= 1000, s"requests param too large, should be <= 1000: $requests")

  private val startTimes = {
    val onePeriodAgo = clock.deadline() - period
    Array.fill(requests)(onePeriodAgo)
  }
  private var position = 0

  private def lastTime: Deadline = startTimes(position)

  private def enqueue(time: Deadline): Unit = {
    startTimes(position) = time
    position += 1
    if (position == requests) position = 0
  }

  def call[T](f: => Future[T]): Future[T] = synchronized {
    val now = clock.deadline()
    if ((now - lastTime) < period) {
      Future.failed(RateLimitExceeded)
    } else {
      enqueue(now)
      f
    }
  }

  override def toString: String = {
    s"Max of $requests every $period"
  }
}

object RateLimiter {

  case object RateLimitExceeded extends RuntimeException("Rate limit exceeded")

}
