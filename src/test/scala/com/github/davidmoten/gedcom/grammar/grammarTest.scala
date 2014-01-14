package com.github.davidmoten.gedcom.grammar

import org.junit._
import org.junit.Assert._
import java.io.FileInputStream
import java.io.File

@Test
class GrammarTest {

  private def parser = new Parser(new FileInputStream(new File("src/main/grammar/gedcom-5.5.1.grammar")))
  
  @Test
  def testParse {
    parser.parseLines.foreach(println)
  }
  
  @Test
  def testParseReturnsGrammarTree {
    println(parser.parse)
  }
  
}
