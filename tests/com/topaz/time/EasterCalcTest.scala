package com.topaz.time

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EasterCalcTest extends AnyFunSuite with Matchers {

  test("easter cal") {
    val byYear = easters.map(d => d.containingYear -> d).toMap
    (Year(1990) to Year(2099)).foreach {
      year =>
        EasterCalc.easterSunday(year.ordinal) shouldEqual byYear(year)
    }
  }
  
  // from https://www.census.gov/srd/www/genhol/easter500.txt
  val easters = Seq(
    Day(1990,4,15),
    Day(1991,3,31),
    Day(1992,4,19),
    Day(1993,4,11),
    Day(1994,4,3),
    Day(1995,4,16),
    Day(1996,4,7),
    Day(1997,3,30),
    Day(1998,4,12),
    Day(1999,4,4),
    Day(2000,4,23),
    Day(2001,4,15),
    Day(2002,3,31),
    Day(2003,4,20),
    Day(2004,4,11),
    Day(2005,3,27),
    Day(2006,4,16),
    Day(2007,4,8),
    Day(2008,3,23),
    Day(2009,4,12),
    Day(2010,4,4),
    Day(2011,4,24),
    Day(2012,4,8),
    Day(2013,3,31),
    Day(2014,4,20),
    Day(2015,4,5),
    Day(2016,3,27),
    Day(2017,4,16),
    Day(2018,4,1),
    Day(2019,4,21),
    Day(2020,4,12),
    Day(2021,4,4),
    Day(2022,4,17),
    Day(2023,4,9),
    Day(2024,3,31),
    Day(2025,4,20),
    Day(2026,4,5),
    Day(2027,3,28),
    Day(2028,4,16),
    Day(2029,4,1),
    Day(2030,4,21),
    Day(2031,4,13),
    Day(2032,3,28),
    Day(2033,4,17),
    Day(2034,4,9),
    Day(2035,3,25),
    Day(2036,4,13),
    Day(2037,4,5),
    Day(2038,4,25),
    Day(2039,4,10),
    Day(2040,4,1),
    Day(2041,4,21),
    Day(2042,4,6),
    Day(2043,3,29),
    Day(2044,4,17),
    Day(2045,4,9),
    Day(2046,3,25),
    Day(2047,4,14),
    Day(2048,4,5),
    Day(2049,4,18),
    Day(2050,4,10),
    Day(2051,4,2),
    Day(2052,4,21),
    Day(2053,4,6),
    Day(2054,3,29),
    Day(2055,4,18),
    Day(2056,4,2),
    Day(2057,4,22),
    Day(2058,4,14),
    Day(2059,3,30),
    Day(2060,4,18),
    Day(2061,4,10),
    Day(2062,3,26),
    Day(2063,4,15),
    Day(2064,4,6),
    Day(2065,3,29),
    Day(2066,4,11),
    Day(2067,4,3),
    Day(2068,4,22),
    Day(2069,4,14),
    Day(2070,3,30),
    Day(2071,4,19),
    Day(2072,4,10),
    Day(2073,3,26),
    Day(2074,4,15),
    Day(2075,4,7),
    Day(2076,4,19),
    Day(2077,4,11),
    Day(2078,4,3),
    Day(2079,4,23),
    Day(2080,4,7),
    Day(2081,3,30),
    Day(2082,4,19),
    Day(2083,4,4),
    Day(2084,3,26),
    Day(2085,4,15),
    Day(2086,3,31),
    Day(2087,4,20),
    Day(2088,4,11),
    Day(2089,4,3),
    Day(2090,4,16),
    Day(2091,4,8),
    Day(2092,3,30),
    Day(2093,4,12),
    Day(2094,4,4),
    Day(2095,4,24),
    Day(2096,4,15),
    Day(2097,3,31),
    Day(2098,4,20),
    Day(2099,4,12))
}
