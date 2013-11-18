package sample

import com.computableideas.lib.extractors.net._

object NetSample {
  def main(args: Array[String]) {
    println("Net Sample")

    "1.2.3.4" match {
      case IP4(a, b, c, d) => println(s"Found the IP4 address $a.$b.$c.$d")
    }

    List("10.0.0.0", "128.0.0.1", "192.168.0.1", "224.1.2.3", "255.255.255.255", "abracadabra") map { s =>
      s match {
        case IP4_ClassA(a, b, c, d) => println(s"$s - Class A found: Net part $a, Host part $b.$c.$d")
        case IP4_ClassB(a, b, c, d) => println(s"$s - Class B found: Net part $a.$b, Host part $c.$d")
        case IP4_ClassC(a, b, c, d) => println(s"$s - Class C found: Net part $a.$b.$c, Host part $d")
        case IP4_ClassD(a, b, c, d) => println(s"$s - Class D found: $a.$b.$c.$d")
        case IP4_ClassE(a, b, c, d) => println(s"$s - Class E found: $a.$b.$c.$d")
        case _ => println(s"$s - Not an IP4")
      }
    }
  }
}

