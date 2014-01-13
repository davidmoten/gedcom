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
case object Unbounded extends MultiplicityBound
case class Multiplicity(min: MultiplicityBound, max: MultiplicityBound)
case class Tag(value: String)
case class Value(depth: Depth, tag: Tag,
  name: String, mult: Multiplicity) extends Element
case class Id(depth: Depth, tag: Tag, id: String,
  mult: Multiplicity) extends Element
case class IdReference(depth: Depth, tag: Tag, id: String,
  mult: Multiplicity) extends Element
case class Definition(name: String, node: Node)
case class DefinitionLine(name: String) extends Element
case class Node(level: Int, element: Element, children: Node)
case class DefinitionReference(depth: Depth, name: String, mult: Multiplicity) extends Element
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

private object Grammar {

  import java.util.regex._
  import RegexHelper._

  private object Patterns {

    private val depthPattern = "^\\s*(0|n|(?:\\+\\d+))\\s+"
    private val multPatternEnding = "\\s*\\{(\\d):(\\d|M)\\}\\s*$"

    val ValuePattern = (
      depthPattern +
      "(\\w+)\\s+<(\\w+)>" +
      multPatternEnding).r

    val IdPattern = (
      depthPattern +
      "@([^@]+)@\\s+(\\w+)" +
      multPatternEnding).r

    val IdReferencePattern = (
      depthPattern +
      "(\\w+)\\s+@([^@]+)@" +
      multPatternEnding).r

    val DefinitionPattern = "^\\s*(\\w+): =\\s*$".r

    val DefinitionReferencePattern = (
      depthPattern +
      "<<(\\w+)>>" +
      multPatternEnding).r
  }

  private def toBound(s: String): MultiplicityBound =
    s match {
      case "M" => Unbounded
      case _ => Specific(s.toInt)
    }

  private def toMultiplicity(min: String, max: String) =
    Multiplicity(toBound(min), toBound(max))

  private def toDepth(s: String) = {
    if (s == "0")
      ZeroDepth
    else if (s == "n")
      AnyDepth
    else
      RelativeDepth(s.substring(1).toInt)
  }

  def parse(line: String): Option[Element] = {
    import Patterns._
    line match {
      case ValuePattern(depth, tag, value, min, max) =>
        Some(Value(toDepth(depth), Tag(tag), value, toMultiplicity(min, max)))
      case DefinitionPattern(name) =>
        Some(DefinitionLine(name))
      case IdPattern(depth, id, tag, min, max) =>
        Some(Id(toDepth(depth), Tag(tag), id, toMultiplicity(min, max)))
      case IdReferencePattern(depth, tag, id, min, max) =>
        Some(IdReference(toDepth(depth), Tag(tag), id, toMultiplicity(min, max)))
      case DefinitionReferencePattern(depth, name, min, max) =>
        Some(DefinitionReference(toDepth(depth), name, toMultiplicity(min, max)))
      case _ => None
    }
  }
}

class Parser(is: java.io.InputStream) {

  def parse = {
    io.Source.fromInputStream(is)
      .getLines
      .filter(_.trim.length > 0)
      .flatMap(line => {
        println(line);
        val elem = Grammar.parse(line);
        println(elem); elem
      })
  }

}