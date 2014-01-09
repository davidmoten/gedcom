package com.github.davidmoten.gedcom

import org.junit._
import org.junit.Assert._

@Test
class ParserTest {

  @Test
  def testParserOnSimpleFile {
    val p = new Tree(getClass.getResourceAsStream("/simple.ged"))
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
   Parser.parse(getClass.getResourceAsStream("/allged.ged"))
//    println(t.format)
  }

   @Test
  def testParserOnSampleFileFrom551StandardPdf {
    Parser.parse(getClass.getResourceAsStream("/sample-from-5.5.1-standard.ged"))
  }
  
}

@Test
class LineParserTest {

  @Test
  def testLineParser {
    val record = Line.parse("2 PLAC death place")
    assertTrue(record.isDefined)
    val rec = record.get
    assertEquals(2,rec.level)
    assertFalse(rec.id.isDefined)
    assertEquals("Line(2,None,PLAC,None,Some(death place))",rec.toString)
  }

  @Test
  def testLineParserId {
    val record = Line.parse("0 @FATHER@ INDI")
    assertTrue(record.isDefined)
    val rec = record.get
    assertEquals(0,rec.level)
    assertTrue(rec.id.isDefined)
    assertEquals("FATHER",rec.id.get)
  }
}
