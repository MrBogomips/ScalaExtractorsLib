package com.computableideas.lib.extractors.net

import java.net.{InetAddress => jIP}

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
      case ip4re(a0, b0, c0, d0) => for{
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
  
  object doesMatch {
    def unapply(s: String): Boolean = IP4.unapply(s).isDefined
    def unapply(ip4: jIP): Boolean = IP4.unapply(ip4).isDefined
  }
}

private[net] trait IP4_Class {
  def checkClass(a: Int, b: Int, c: Int, d: Int): Option[(Net, Host)]

  def unapply(s: String): Option[(Net, Host)] = s match {
    case IP4(a,b,c,d) => checkClass(a,b,c,d)
    case _ => None
  }
 
  def unapply(ip4: jIP): Option[(Net, Host)] = ip4 match {
    case IP4(a,b,c,d) => checkClass(a,b,c,d)
    case _ => None
  }
}

object IP4_ClassC extends IP4_Class {
  def checkClass(a: Int, b: Int, c: Int, d: Int) = if ((a & 0xE0) >> 5 == 6)
  Some((Net(a :: b :: c :: Nil), Host( d :: Nil)))
  else
  None
}


object IP4_ClassB extends IP4_Class {
  def checkClass(a: Int, b: Int, c: Int, d: Int) = if ((a & 0xC0) >> 6 == 2)
  Some((Net(a :: b :: Nil), Host(c :: d :: Nil)))
  else
  None
}


object IP4_ClassA extends IP4_Class{
  def checkClass(a: Int, b: Int, c: Int, d: Int) = if ((a & 0x80) >> 7 == 0)
  Some((Net(a :: Nil), Host(b :: c :: d :: Nil)))
  else
  None
}

