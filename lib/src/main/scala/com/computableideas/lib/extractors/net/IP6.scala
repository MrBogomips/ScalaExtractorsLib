package com.computableideas.lib.extractors.net

import java.net.{ InetAddress => jIP }

/**
 * IP Address v4 extractor.
 *
 *
 */
object IP6 {
  /**
   * String extractor
   *
   * This extractor doesn't rely on java.net IP4 parsing functions
   * resulting faster
   */
  def unapply(s: String): Option[(Int, Int, Int, Int, Int, Int, Int, Int)] = {
    ???
  }
  /**
   * InetAddress extractor
   */
  def unapply(ip6: jIP): Option[(Int, Int, Int, Int, Int, Int, Int, Int)] = {
    val ip = ip6.getAddress()
    if (ip.length == 16)
      Some(
        ((ip(0) & 0x0FF) << 8 | (ip(1) & 0x0FF)),
        ((ip(2) & 0x0FF) << 8 | (ip(3) & 0x0FF)),
        ((ip(4) & 0x0FF) << 8 | (ip(5) & 0x0FF)),
        ((ip(6) & 0x0FF) << 8 | (ip(7) & 0x0FF)),
        ((ip(8) & 0x0FF) << 8 | (ip(9) & 0x0FF)),
        ((ip(10) & 0x0FF) << 8 | (ip(11) & 0x0FF)),
        ((ip(12) & 0x0FF) << 8 | (ip(13) & 0x0FF)),
        ((ip(14) & 0x0FF) << 8 | (ip(15) & 0x0FF)))
    else
      None
  }
}