package com.computableideas.lib.extractors.test

import java.net.{InetAddress => jIP}

import com.computableideas.lib.extractors.net._

class NetSpec extends CommonSuite {
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
	"Java Inet 6 Addresses" should "not be extracted through IP4 extractor" in {
	  val ip1: jIP = jIP.getByName("2001:0db8:85a3:0000:0000:8a2e:0370:7334")
	  assert(IP4.unapply(ip1) === None)
	}
	"Java Inet 6 Addresses" should "be extracted through IP6 extractor" in {
	  val ip1: jIP = jIP.getByName("2001:0db8:85a3:0000:0000:8a2e:0370:7334")
	  assert(IP6.unapply(ip1) === Some(0x2001, 0x0db8, 0x85a3, 0x0000, 0x0000, 0x8a2e, 0x0370, 0x7334))
	  
	  val ip2: jIP = jIP.getByName("FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF")
	  assert(IP6.unapply(ip2) === Some(0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF))
	}
	"Java Inet 4 Addresses" should "not be extracted through IP6 extractor" in {
	  val ip1: jIP = jIP.getByName("1.2.3.4")
	  assert(IP6.unapply(ip1) === None)
	}
}