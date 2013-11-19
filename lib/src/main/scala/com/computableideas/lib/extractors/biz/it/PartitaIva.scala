package com.computableideas.lib.extractors.biz.it

/**
 * Partita Iva Extractor
 */
object PartitaIva {
  private val oddMap = Map(
    0 -> 0,
    1 -> 2,
    2 -> 4,
    3 -> 6,
    4 -> 8,
    5 -> 1,
    6 -> 3,
    7 -> 5,
    8 -> 7,
    9 -> 9)
  private val c0 = '0'.toInt

  def unapply(s: String): Boolean =
    s.length() == 11 &&
      s.forall(_.isDigit) &&
      s.zipWithIndex.foldLeft(0) { (acc, p) =>
        if (p._2 % 2 == 0) acc + p._1 - c0
        else acc + oddMap(p._1 - c0)
      } % 10 == 0
}