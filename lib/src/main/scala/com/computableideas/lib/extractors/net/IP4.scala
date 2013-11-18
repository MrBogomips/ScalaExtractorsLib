package com.computableideas.lib.extractors.net

import java.net.{ InetAddress => jIP }

/**
 * IP Address v4 extractor.
 *
 *
 */
object IP4 {
  /**
   * String extractor
   *
   * This extractor doesn't rely on java.net IP4 parsing functions
   * resulting faster
   */
  def unapply(s: String): Option[(Int, Int, Int, Int)] = {
    val ip4re = """(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})""".r
    def s2b(s: String): Option[Int] = {
      val b = Integer.parseInt(s)
      if (b < 256) Some(b) else None
    }
    s match {
      case ip4re(a0, b0, c0, d0) => for {
        a1 <- s2b(a0)
        b1 <- s2b(b0)
        c1 <- s2b(c0)
        d1 <- s2b(d0)
      } yield (a1, b1, c1, d1)
      case _ => None
    }
  }
  /**
   * InetAddress extractor
   */
  def unapply(ip4: jIP): Option[(Int, Int, Int, Int)] = {
    val ip = ip4.getAddress()
    if (ip.length == 4)
      Some(ip(0), ip(1), ip(2), ip(3))
    else
      None
  }
}

private[net] trait IP4_Class {
  def checkClass(a: Int, b: Int, c: Int, d: Int): Boolean

  def unapply(s: String): Option[(Int, Int, Int, Int)] = s match {
    case IP4(a, b, c, d) if checkClass(a, b, c, d) => Some(a, b, c, d)
    case _ => None
  }

  def unapply(ip4: jIP): Option[(Int, Int, Int, Int)] = ip4 match {
    case IP4(a, b, c, d) if checkClass(a, b, c, d) => Some(a, b, c, d)
    case _ => None
  }
}

object IP4_ClassA extends IP4_Class {
  def checkClass(a: Int, b: Int, c: Int, d: Int) = (a & 0x80) >> 7 == 0
}

object IP4_ClassB extends IP4_Class {
  def checkClass(a: Int, b: Int, c: Int, d: Int) = (a & 0xC0) >> 6 == 2
}

object IP4_ClassC extends IP4_Class {
  def checkClass(a: Int, b: Int, c: Int, d: Int) = (a & 0xE0) >> 5 == 6
}

object IP4_ClassD extends IP4_Class {
  def checkClass(a: Int, b: Int, c: Int, d: Int) = (a & 0xF0) == 0xE0
}

object IP4_ClassE extends IP4_Class {
  def checkClass(a: Int, b: Int, c: Int, d: Int) = (a & 0xF0) == 0xF0
}
