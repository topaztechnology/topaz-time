package com.topaz.time

import com.topaz.TopazCodingError
import com.topaz.time.Tenor.Months
import com.topaz.utils.{CollectionUtils, ParseInt}
import enumeratum.{Enum, EnumEntry}

import scala.collection.immutable

sealed abstract class TenorType(override val entryName: String) extends EnumEntry with CollectionUtils

object TenorType extends Enum[TenorType] {
  val values: immutable.IndexedSeq[TenorType] = findValues
  val wholeMonthValues: Seq[TenorType] = Seq(MonthsTenorType, YearsTenorType)

  case object OvernightTenorType extends TenorType("Overnight")
  case object WeeksTenorType extends TenorType("Weeks")
  case object MonthsTenorType extends TenorType("Months")
  case object YearsTenorType extends TenorType("Years")
}


sealed trait Tenor {
  def +(that: Tenor): WholeMonthTenor = throw TopazCodingError(s"Can't add tenors $this and $that")
  def reportText: String = persistentString
  def persistentString: String
}
sealed trait WholeMonthTenor extends Tenor {
  def numMonths: Int

  override def +(that: Tenor) = that match {
    case wmt : WholeMonthTenor => Months(numMonths + wmt.numMonths)
    case _ => super.+(that)
  }
}

object Tenor {

  private val Pattern = """(\d+)([wmyWMY])""".r

  def fromString(str: String): Option[Tenor] = {
    str match {
      case "ON" | "on" => Some(Overnight)
      case Pattern(ParseInt(n), typ) =>
        typ.toLowerCase match {
          case "w" => Some(Weeks(n))
          case "m" => Some(Months(n))
          case "y" => Some(Years(n))
          case o => throw TopazCodingError(s"Unexpected type: '$o''")
        }
      case _ => None
    }
  }

  case object Overnight extends Tenor {
     def persistentString: String = "ON"
  }

  case class Weeks(weeks: Int) extends Tenor {
     def persistentString: String = s"${weeks}W"
  }

  case class Months(months: Int) extends WholeMonthTenor {
    def numMonths: Int = months

    def persistentString: String = s"${months}M"

    override def reportText: String = {
      if (months > 12) {
        val y = months / 12
        val m = months % 12
        s"${y}Y${m}M"
      } else
        s"${months}M"
    }
  }

  case class Years(years: Int) extends WholeMonthTenor {
    def numMonths: Int = years * 12

    def persistentString: String = s"${years}Y"
  }

  val ordering: Ordering[Tenor] = new Ordering[Tenor] {
    def compare(x: Tenor, y: Tenor): Int = {
      (x, y) match {
        case (Overnight, Overnight) => 0
        case (Weeks(n), Weeks(m)) => n.compareTo(m)
        case (Months(n), Months(m)) => n.compareTo(m)
        case (Years(n), Years(m)) => n.compareTo(m)
        case _ =>
          val order: immutable.Seq[Class[_]] = Vector(
            Overnight.getClass,
            classOf[Weeks],
            classOf[Months],
            classOf[Years],
          )
          order.indexOf(x.getClass).compareTo(order.indexOf(y.getClass))
      }
    }
  }
}
