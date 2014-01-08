package com.github.davidmoten.gedcom

import org.junit._
import org.junit.Assert._

@Test
class ParserTest {

  @Test
  def testParserOnSimpleFile {
    val t = new GedcomParser().parse(getClass.getResourceAsStream("/simple.ged"))
    println(t.format)
  }

  @Test
  def testParserOnComplexFile {
   // println(new GedcomParser().parse(getClass.getResourceAsStream("/allged.ged")).format)
  }

}

@Test
class RecordParserTest {

  @Test
  def testRecordParser {
    val record = Record.parse("2 PLAC death place")
    assertTrue(record.isDefined)
    val rec = record.get
    assertEquals(2,rec.level)
    assertFalse(rec.id.isDefined)
    assertEquals("Record(2,None,PLAC,None,Some(death place))",rec.toString)
  }

  @Test
  def testRecordParserId {
    val record = Record.parse("0 @FATHER@ INDI")
    assertTrue(record.isDefined)
    val rec = record.get
    assertEquals(0,rec.level)
    assertTrue(rec.id.isDefined)
    assertEquals("FATHER",rec.id.get)
  }
}
