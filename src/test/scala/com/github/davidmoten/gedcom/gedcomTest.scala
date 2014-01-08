package com.github.davidmoten.gedcom

import org.junit._
import org.junit.Assert._

@Test
class ParserTest {

  @Test
  def testParserOnSimpleFile {
    val p = new Parser(getClass.getResourceAsStream("/simple.ged"))
    println(p.root.format)
    println(p.refs.mkString("\n"))
    assertTrue(p.refs.keySet.contains("MOTHER"))
    assertTrue(p.refs.keySet.contains("FATHER"))
    assertTrue(p.refs.keySet.contains("CHILD"))
    assertTrue(p.refs.keySet.contains("SUBMITTER"))
    assertTrue(p.refs.keySet.contains("FAMILY"))
  }

  @Test
  def testParserOnComplexFile {
    val t = Parser.parse(getClass.getResourceAsStream("/allged.ged"))
//    println(t.format)
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
