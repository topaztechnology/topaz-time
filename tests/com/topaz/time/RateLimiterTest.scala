package com.topaz.time

import akka.Done
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.Future
import scala.concurrent.duration._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RateLimiterTest extends AnyFunSuite with Matchers with ScalaFutures {

  test("test limit") {
    def f = Future.successful(Done)
    
    implicit val clock = new MutableClock(0)

    val limit = new RateLimiter(3, 10.seconds)
    
    limit.call(f).futureValue shouldBe Done
    clock += 3.seconds
    limit.call(f).futureValue shouldBe Done
    clock += 3.seconds
    limit.call(f).futureValue shouldBe Done
    clock += 3.seconds
    limit.call(f).failed.futureValue shouldBe RateLimiter.RateLimitExceeded
    clock += 1.second
    limit.call(f).futureValue shouldBe Done
  }
}
