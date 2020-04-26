package com.topaz.time


import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.util.Random

trait DateRangeTestsMixin extends Matchers with AnyFunSpecLike {

  def orderingIsSatisfied(cs: DateRange*) {

    it("compared to itself should be 0") {
      cs.foreach { c =>
        c.compare(c) should be (0)
      }
    }

    it("should be before next") {
      cs.foreach { c =>
        c.compare(c + 1) should be < 0
      }
    }

    it("should be after previous") {
      cs.foreach { c =>
        c.compare(c - 1) should be > 0
      }
    }

  }

  def sortingWorksCorrectly(sortedCs: Seq[DateRange]) {
    sortedCs.sorted shouldEqual sortedCs
    Random.shuffle(sortedCs).sorted shouldEqual sortedCs
  }

  def rangesAreOfCorrectSize(cs: DateRange*) {
    it("range from instance to itself has size 1") {
      cs.foreach { c =>
        (c to c).size should be (1)
      }
    }

    it("range from instance to next has size 2") {
      cs.foreach { c =>
        (c to c.next).size should be (2)
      }
    }

    it("ranges cannot be constructed with an earlier instance") {
      cs.foreach { c =>
        intercept[IllegalArgumentException] {
          c to c.previous
        }
      }
    }
  }

  def ordinalsCanBeRoundTripped(cs: DateRange*) {
    it("should round trip ordinals") {
      cs.foreach { c => 
        c.dateRangeType.fromOrdinal(c.ordinal) should equal (c)
      }
    }
  }

  def addAndSubtractMatchesNextAndPrev(cs: DateRange*) {
    it("should add and subtract to match next and prev") {
      cs.foreach {c =>
        val next = Iterator.iterate(c)(_ + 1)
        (0 to 100).foreach(i => c + i shouldEqual next.next)
        val prev = Iterator.iterate(c)(_ - 1)
        (0 to 100).foreach(i => c - i shouldEqual prev.next)
      }
    }
  }

  def stringsCanBeRoundTripped(cs: DateRange*) {
    it("should round trip strings") {
      cs.foreach {c =>
        c.dateRangeType.unapply(c.toString).get should equal(c)
      }
    }
  }

  def chronologicalDateRange(cs: DateRange*) {
    it should behave like orderingIsSatisfied(cs: _*)

    it should behave like ordinalsCanBeRoundTripped(cs: _*)

    it should behave like rangesAreOfCorrectSize(cs: _*)

    it should behave like addAndSubtractMatchesNextAndPrev(cs: _*)

    it should behave like stringsCanBeRoundTripped(cs: _*)
  }
}
