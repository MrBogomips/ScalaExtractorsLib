package com.computableideas.lib.extractors.test

import java.net.{InetAddress => jIP}

import com.computableideas.lib.extractors.net._

class NetSpec extends CommonSuite {
	//////////////////////   IP 6
	"Valid IP4 addresses" should "be extracted" in {
		assert(IP4.unapply("0.0.0.0") === Some(0,0,0,0))
		assert(IP4.unapply("0.0.0.1") === Some(0,0,0,1))
		assert(IP4.unapply("0.0.1.2") === Some(0,0,1,2))
		assert(IP4.unapply("0.1.2.3") === Some(0,1,2,3))
		assert(IP4.unapply("1.2.3.4") === Some(1,2,3,4))
		assert(IP4.unapply("255.255.255.255") === Some(255,255,255,255))
	}

	"Invalid IP4 addresses" should "not be extracted" in {
		assert(IP4.unapply("abracadabra") === None)
		assert(IP4.unapply("") === None)
		assert(IP4.unapply("-1.0.1.2") === None)
		assert(IP4.unapply("256.255.255.255") === None)
	}

	"Java Inet 4 Addresses" should "be extracted through IP4 extractor" in {
		val ip1: jIP = jIP.getByName("1.2.3.4")
		assert(IP4.unapply(ip1) === Some(1,2,3,4))
	}

	"IP4 of a Class A Network" should "be extracted from 0.0.0.0" in {
		val ip = "0.0.0.0"
		val (net, host) = IP4_ClassA.unapply(ip).get
		assert(net.parts === List(0))
		assert(host.parts === List(0, 0, 0)) 
	} 
	it should "be extracted until to 127.255.255.255" in {
		val ip = "127.255.255.255"
		val (net, host) = IP4_ClassA.unapply(ip).get
		assert(net.parts === List(127))
		assert(host.parts === List(255, 255, 255)) 
	}
	it should "not be extracted from 128.0.0.0" in {
		IP4_ClassA.unapply("128.0.0.0") === None
	}
	it should "not be extracted until to 255.255.255.255" in {
		IP4_ClassA.unapply("255.255.255.255") === None
	}

	"IP4 of a Class B Network" should "be extracted from 128.0.0.0" in {
		val ip = "128.0.0.0"
		val (net, host) = IP4_ClassB.unapply(ip).get
		assert(net.parts === List(128,0))
		assert(host.parts === List(0, 0)) 
	} 
	it should "be extracted until to 191.255.255.255" in {
		val ip = "191.255.255.255"
		val (net, host) = IP4_ClassB.unapply(ip).get
		assert(net.parts === List(191,255))
		assert(host.parts === List(255, 255)) 
	}
	it should "not be extracted from 127.255.255.255" in {
		IP4_ClassB.unapply("127.255.255.255") === None
	}
	it should "not be extracted from 192.0.0.0" in {
		IP4_ClassB.unapply("192.0.0.0") === None
	}
	it should "not be extracted until to 223.255.255.255" in {
		IP4_ClassB.unapply("223.255.255.255") === None
	}

	"IP4 of a Class C Network" should "be extracted from 192.0.0.0" in {
		val ip = "192.0.0.0"
		val (net, host) = IP4_ClassC.unapply(ip).get
		assert(net.parts === List(192, 0, 0))
		assert(host.parts === List(0)) 
	} 
	it should "be extracted until to 223.255.255.255" in {
		val ip = "223.255.255.255"
		val (net, host) = IP4_ClassC.unapply(ip).get
		assert(net.parts === List(223,255, 255))
		assert(host.parts === List(255)) 
	}
	it should "not be extracted from 223.255.255.255" in {
		IP4_ClassC.unapply("223.255.255.255") === None
	}
	it should "not be extracted from 224.0.0.0" in {
		IP4_ClassC.unapply("224.0.0.0") === None
	}
	it should "not be extracted until to 239.255.255.255" in {
		IP4_ClassC.unapply("239.255.255.255") === None
	}

	/*
	"IP4 of a Class B Network" should "be extracted properly" in {
		val ip = "255.255.255.255"
		val (net, host) = IP4_ClassB.unapply(ip).get
		assert(net.parts === List(255, 240))
		assert(host.parts === List(15, 255, 255))
	}
	"IP4 of a Class C Network" should "be extracted properly" in {
		val ip = "1.2.3.4"
		val (net, host) = IP4_ClassC.unapply(ip).get
		assert(net.parts === List(1, 2, 3))
		assert(host.parts === List(4))
	}
*/

	//////////////////////   IP 6
	"Java Inet 6 Addresses" should "not be extracted through IP4 extractor" in {
		val ip1: jIP = jIP.getByName("2001:0db8:85a3:0000:0000:8a2e:0370:7334")
		assert(IP4.unapply(ip1) === None)
	}
	"Java Inet 6 Addresses" should "be extracted through IP6 extractor" in {
		val ip1: jIP = jIP.getByName("2001:0db8:85a3:0000:0000:8a2e:0370:7334")
		assert(IP6.unapply(ip1) === Some(0x2001, 0xdb8, 0x85a3, 0, 0, 0x8a2e, 0x0370, 0x7334))

		val ip2: jIP = jIP.getByName("FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF")
		assert(IP6.unapply(ip2) === Some(0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF))
	}
	"Java Inet 4 Addresses" should "not be extracted through IP6 extractor" in {
		val ip1: jIP = jIP.getByName("1.2.3.4")
		assert(IP6.unapply(ip1) === None)
	}
}