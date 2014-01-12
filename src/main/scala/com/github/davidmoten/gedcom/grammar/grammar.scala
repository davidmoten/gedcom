package com.github.davidmoten.gedcom.grammar

trait Element
sealed trait Depth
case object AnyDepth extends Depth
case object ZeroDepth extends Depth
case class RelativeDepth(depth: Int) extends Depth
sealed trait MultiplicityBound
case class Specific(value: Integer) extends MultiplicityBound {
  require(value >= 0)
}
case class Unbounded extends MultiplicityBound
case class Multiplicity(min: MultiplicityBound, max: MultiplicityBound)
case class Tag(value: String)
case class Value(depth: Depth, tag: Tag,
  name: String, mult: Multiplicity) extends Element
case class Id(depth: Depth, tag: Tag, id: String) extends Element
case class Definition(name: String, node: Node)
case class DefinitionLine(name: String) extends Element
case class Node(level: Int, element: Element, children: Node)
case class DefinitionReference(depth: Depth, name: String) extends Element
case class Grammar(root: Definition, definitions: List[Definition])

object RegexHelper {
  import java.util.regex._

  def matcher(pattern: Pattern, line: String): (Option[String], Option[Matcher]) = {
    val m = pattern.matcher(line)
    if (!m.find)
      (Some(line), None)
    else
      (None, Some(m))
  }
}

private object Line {

  import java.util.regex._
  import RegexHelper._

  private val multPatternEnding = "\\s*{(\\d:(\\d|M))}\\s*$"

  private val ValuePattern =
    ("^\\s*(0|n|(\\+\\d+))\\s+(\\w+)\\s+<(\\w+)>" +
      multPatternEnding).r

  private val IdPattern = ("^\\s*(0|n|(\\+\\d+))\\s+(\\w+)" +
    multPatternEnding).r

  private val DefinitionPattern = "^\\s*(\\w+): =\\s*$".r

  private val DefinitionReferencePattern = (
    "^\\s*(0|n|(\\+\\d+))\\s+<<(\\w+)>>" +
    multPatternEnding).r

  private def parseDefinitionLine(line: String) = {
    DefinitionPattern.findFirstMatchIn(line).map(m => DefinitionLine(m.group(1)))
  }

  private def parseValue(line: String) = {
    ValuePattern.findFirstMatchIn(line).map(
      m => Value(AnyDepth, Tag("SOUR"), "fred", Multiplicity(Specific(1), Unbounded())))
  }

  private def toDepth(s: String) = {
    if (s == "0")
      ZeroDepth
    else if (s == "n")
      AnyDepth
    else 
      RelativeDepth(s.substring(1).toInt)
  }

//  private def parseId(line: String) = {
//    IdPattern.findFirstMatchIn(line).map(m => Id(toDepth(m.group(1)),Tag(m.group(3)),)
//  }

  //1=relative level,4=def ref, 6=tag, 9=xref,10=name, 11=min,12=max
  def parse(line: String) = {
    (parseDefinitionLine(line) ++
      parseValue(line))
      .headOption
  }

  def parse(is: java.io.InputStream) = {
    io.Source.fromInputStream(is)
      .getLines

    null
  }
}

class Parser(is: java.io.InputStream) {

  def parse = {
    val empty: (List[List[String]], List[String]) = (List(), List())
    io.Source.fromInputStream(is)
      .getLines
      .filter(_.trim.length > 0)
      .foldLeft(empty)((g, line) => g)
  }

}