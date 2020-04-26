package com.topaz.time

import com.topaz.utils.{RandomGeneratorPimps, RandomTestSuite}

import scala.collection.immutable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class HourOfDayTest extends AnyFunSuite with Matchers with RandomTestSuite with RandomGeneratorPimps {
  private def range(start: Int, end: Int): immutable.IndexedSeq[NormalHour] = {
    var hours = immutable.IndexedSeq.newBuilder[NormalHour]
    var hour = start
    while (hour != end) {
      hours += HourOfDay.all(hour)
      hour = (hour + 1) % 24
    }
    hours += HourOfDay.all(hour) // inclusive
    hours.result()
  }

  test("range") {
    HourOfDay.range(0, 6) ++ HourOfDay.range(23, 23) should contain theSameElementsAs range(23, 6)
  }
  
  test("to and from strings") {
    val seq = List(1, 2, 3, 4, 6, 8, 9, 10, 12)
    val hours = seq.map(NormalHour)
    
    HourOfDay.toString(hours.toSet) shouldEqual "1-4,6,8-10,12"
    
    HourOfDay.fromString("1-4,6,8-10,12") shouldEqual hours.toSet
    
    withRandomGenerator(numTests = 10) {
      rng =>
        val hours = rng.seqOfThings(NormalHour(rng.nextInt(23)), rng.nextInt(30)).distinct
        val string = HourOfDay.toString(hours.toSet)
        HourOfDay.fromString(string) shouldEqual hours.toSet
    }
  }
}
