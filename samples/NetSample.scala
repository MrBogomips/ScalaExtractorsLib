package sample

import com.computableideas.lib.extractors.net._

object NetSample {
  def main(args: Array[String]) {
    println("Net Sample")
    
    "1.2.3.4" match {
      case IP4(a, b, c, d) => println(s"Found the IP4 address $a.$b.$c.$d")
    } 
    
    "1.2.3.4" match {
      case ip @ IP4.doesMatch() => println(s"$ip looks like a valid IP4 address")
    }

    "192.168.0.1" match {
      case IP4_ClassC(net, host) => println(s"Host $host belongs to net $net")
    }
  }
}

