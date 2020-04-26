package com.topaz.time

import com.topaz.utils.RandomTestSuite

import scala.util.Random
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RiskPeriodTest extends AnyFunSuite with Matchers with RandomTestSuite {

  test("sorting spreads") {
    // #138032061 - spreads sort after date ranges
    val periods = Seq(
      SpotRisk,
      DateRangeRiskPeriod(Year(2016)),
      DateRangeRiskPeriod(Month(2016, 1)),
      DateRangeRiskPeriod(Month(2016, 2)),
      DateRangeRiskPeriod(Year(2025)),
      SpreadRiskPeriod(Month(2015, 2), Month(2015, 2)),
      SpreadRiskPeriod(Month(2017, 2), Month(2018, 10)),
      SpreadRiskPeriod(Month(2017, 2), Month(2018, 11))
    )

    Random.shuffle(periods).sorted(implicitly[Ordering[RiskPeriod]]) shouldEqual periods
  }

  test("sorting initial/terminal") {
    withRandomGenerator(numTests = 5) {
      rng =>
        val periods = Seq(
          InitialStorageRiskPeriod(Month(2016, 1)),
          DateRangeRiskPeriod(Month(2016, 1)),
          TerminalStorageRiskPeriod(Month(2016, 1)),
          DateRangeRiskPeriod(Month(2016, 2)),
          InitialStorageRiskPeriod(Month(2016, 3))
        )

        val rnd = new Random(rng.nextLong())
        rnd.shuffle(periods).sorted(implicitly[Ordering[RiskPeriod]]) shouldEqual periods
    }
  }
}
