package sample

import com.computableideas.lib.extractors.biz.it.PartitaIva

object PartitaIvaSample {
  def main(args: Array[String]) {
    println("Partita Iva Sample")

    List("00488410010", "06382641006", "00488410011", "abracadabra") map { s =>
      s match {
        case piva @ PartitaIva() => println(s"$piva is a valid Partita Iva")
        case piva @ _ => println(s"$piva is not a valid Partita Iva")
      }
    }
  }
}
