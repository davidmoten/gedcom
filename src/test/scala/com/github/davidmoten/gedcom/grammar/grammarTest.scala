package com.github.davidmoten.gedcom.grammar

import org.junit._
import org.junit.Assert._

@Test
class GrammarTest {

  @Test
  def testParse {
    import java.io._
    val parser = new Parser(new FileInputStream(new File("src/main/grammar/gedcom-5.5.1.grammar")))
    parser.parse.foreach(println)
  }
  
}
