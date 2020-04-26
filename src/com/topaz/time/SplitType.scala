package com.topaz.time

import enumeratum._
import scala.collection.immutable

/**
  * Describes how to split a date range according to some DateRangeType
  */
sealed abstract class SplitType(override val entryName: String) extends EnumEntry
object SplitType extends Enum[SplitType] {
  val values: immutable.IndexedSeq[SplitType] = findValues

  case object Outer extends SplitType("Outer")
  case object Inner extends SplitType("Inner")
  case object Exact extends SplitType("Exact")
  case object LeftExactRightOuter extends SplitType("LeftExactRightOuter")
  case object LeftOuterRightExact extends SplitType("LeftExactRightOuter")
}

