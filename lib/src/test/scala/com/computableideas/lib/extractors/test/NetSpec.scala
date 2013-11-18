package com.computableideas.lib.extractors.test

import com.computableideas.lib.extractors.net._
import java.net.{InetAddress => jIP}
import org.scalacheck.Gen

class NetSpec extends CommonSuite {
	//////////////////////   IP 4
	"Valid IP4 addresses" should "be extracted" in {
		IP4.unapply("0.0.0.0") should be (Some(0,0,0,0))
		IP4.unapply("0.0.0.1") should be (Some(0,0,0,1))
		IP4.unapply("0.0.1.2") should be (Some(0,0,1,2))
		IP4.unapply("0.1.2.3") should be (Some(0,1,2,3))
		IP4.unapply("1.2.3.4") should be (Some(1,2,3,4))
		IP4.unapply("255.255.255.255") should be (Some(255,255,255,255))
	}

	"Invalid IP4 addresses" should "not be defined" in {
		IP4.unapply("abracadabra") should not be defined
		IP4.unapply("") should not be defined
		IP4.unapply("-1.0.1.2") should not be defined
		IP4.unapply("256.255.255.255") should not be defined
	}

	"Java Inet 4 Addresses" should "be extracted through IP4 extractor" in {
		val ip1: jIP = jIP.getByName("1.2.3.4")
		IP4.unapply(ip1) should be (Some(1,2,3,4))
	}

	"IP4 of a Class A" should "be extracted from 0.0.0.0" in {
		IP4.ClassA.unapply("0.0.0.0") should be (Some(0,0,0,0))
	} 
	it should "be extracted until to 127.255.255.255" in {
		IP4.ClassA.unapply("127.255.255.255") should be (Some(127,255,255,255))
	}
	it should "not be extracted from 128.0.0.0" in {
		IP4.ClassA.unapply("128.0.0.0") should not be defined
	}
	it should "not be extracted until to 255.255.255.255" in {
		IP4.ClassA.unapply("255.255.255.255") should not be defined
	}

	"IP4 of a Class B" should "be extracted from 128.0.0.0" in {
		IP4.ClassB.unapply("128.0.0.0") should be (Some(128, 0, 0, 0))
	} 
	it should "be extracted until to 191.255.255.255" in {
		 IP4.ClassB.unapply("191.255.255.255") should be (Some(191, 255, 255, 255))
	}
	it should "not be extracted from 127.255.255.255" in {
		IP4.ClassB.unapply("127.255.255.255") should not be defined
	}
	it should "not be extracted from 192.0.0.0" in {
		IP4.ClassB.unapply("192.0.0.0") should not be defined
	}
	it should "not be extracted until to 223.255.255.255" in {
		IP4.ClassB.unapply("223.255.255.255") should not be defined
	}

	"IP4 of a Class C" should "be extracted from 192.0.0.0" in {
		IP4.ClassC.unapply("192.0.0.0") should be (Some(192,0,0,0))
	} 
	it should "be extracted until to 223.255.255.255" in {
		IP4.ClassC.unapply("223.255.255.255") should be (Some(223,255,255,255))
	}
	it should "not be extracted from 224.0.0.0" in {
		IP4.ClassC.unapply("224.0.0.0") should not be defined
	}
	it should "not be extracted until to 239.255.255.255" in {
		IP4.ClassC.unapply("239.255.255.255") should not be defined
	}

	"IP4 of a Class D" should "be extracted from 224.0.0.0" in {
		IP4.ClassD.unapply("224.0.0.0") should be (Some(224, 0, 0, 0))
	}
	it should "be extracted until to 239.255.255.255" in {
		IP4.ClassD.unapply("239.255.255.255") should be (Some(239, 255, 255, 255))	
	}
	it should "not be extracted from 240.0.0.0" in {
		IP4.ClassD.unapply("240.0.0.0") should not be defined
	}
	it should "not be extracted from 223.255.255.255" in {
		IP4.ClassD.unapply("223.255.255.255") should not be defined
	}
	"IP4 of a Class E" should "be extracted from 240.0.0.0" in {
		IP4.ClassE.unapply("240.0.0.0") should be (Some(240, 0, 0, 0))	
	}
	it should "be extracted until to 255.255.255.255" in {
		IP4.ClassE.unapply("255.255.255.255") should be (Some(255, 255, 255, 255))		
	}

	"An IP4 address" should "belong to just one class" in {
		val addrs = for {
			a <- Gen.choose(0, 255)
			b <- Gen.choose(0, 255)
			c <- Gen.choose(0, 255)
			d <- Gen.choose(0, 255)
		} yield s"$a.$b.$c.$d"
		forAll (addrs) { ip4s =>
			var c: Int = 0
			if (IP4.ClassA.unapply(ip4s).isDefined) c = c + 1
			if (IP4.ClassB.unapply(ip4s).isDefined) c = c + 1
			if (IP4.ClassC.unapply(ip4s).isDefined) c = c + 1
			if (IP4.ClassD.unapply(ip4s).isDefined) c = c + 1
			if (IP4.ClassE.unapply(ip4s).isDefined) c = c + 1
			c should be (1)
		}
	}


	def ip(s: String) = jIP.getByName(s)

	//////////////////////   IP 6
	"Java Inet 6 Addresses" should "not be extracted through IP4 extractor" in {
		IP4.unapply(ip("2001:0db8:85a3:0000:0000:8a2e:0370:7334")) should not be defined
	}
	"Java Inet 6 Addresses" should "be extracted through IP6 extractor" in {
		IP6.unapply(ip("2001:0db8:85a3:0000:0000:8a2e:0370:7334")) should be (Some(0x2001, 0xdb8, 0x85a3, 0, 0, 0x8a2e, 0x0370, 0x7334))
		IP6.unapply(ip("FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF")) should be (Some(0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF))
	}
	"Java Inet 4 Addresses" should "not be extracted through IP6 extractor" in {
		IP6.unapply(ip("1.2.3.4")) should not be defined
	}
}