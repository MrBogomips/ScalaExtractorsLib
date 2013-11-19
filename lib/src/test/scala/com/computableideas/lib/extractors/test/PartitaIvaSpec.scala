package com.computableideas.lib.extractors.test

import com.computableideas.lib.extractors.biz.it.{PartitaIva => PIva}

class PartitaIvaSpec extends CommonSuite {
  "A valid Partita IVA" should "extract Telecom Itaia PIVA" in {
    PIva.unapply("00488410010") should be (true)
  } 
  it should "extract RAI PIVA" in {
    PIva.unapply("06382641006") should be (true)
  }
  it should """not extract from "00488410011"""" in {
    PIva.unapply("00488410011") should not be (true)
  }
}