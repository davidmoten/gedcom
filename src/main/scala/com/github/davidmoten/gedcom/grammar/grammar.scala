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

  private val multPatternEnding = "\\s*\\{(\\d):(\\d|M)\\}\\s*$"

  private val ValuePattern =
    ("^\\s*(0|n|(\\+\\d+))\\s+(\\w+)\\s+<(\\w+)>" +
      multPatternEnding).r

  private val IdPattern = ("^\\s*(0|n|(\\+\\d+))\\s+@([^@]+)@\\s+(\\w+)" +
    multPatternEnding).r

  private val IdReferencePattern = ("^\\s*(0|n|(\\+\\d+))\\s+(\\w+)\\s+@([^@]+)@" +
    multPatternEnding).r

  private val DefinitionPattern = "^\\s*(\\w+): =\\s*$".r

  private val DefinitionReferencePattern = (
    "^\\s*(0|n|(\\+\\d+))\\s+<<(\\w+)>>" +
    multPatternEnding).r

  private def toBound(s: String): MultiplicityBound =
    s match {
      case "M" => Unbounded
      case _ => Specific(s.toInt)
    }

  private def toMultiplicity(min: String, max: String) =
    Multiplicity(toBound(min), toBound(max))

  private type Input = Either[String, Element]

  private def parseDefinitionLine(input: Input): Input = {
    if (input.isLeft)
      DefinitionPattern
        .findFirstMatchIn(input.left.get)
        .map(
          m => DefinitionLine(m.group(1))) match {
            case Some(e) => Right(e)
            case None => input
          }
    else
      input
  }

  private def parseValueLine(input: Input) = {
    if (input.isLeft)
      ValuePattern
        .findFirstMatchIn(input.left.get)
        .map(
          m => Value(toDepth(m.group(1)), Tag(m.group(3)), m.group(4),
            toMultiplicity(m.group(5), m.group(6)))) match {
            case Some(e) => Right(e)
            case None => input
          }
    else
      input
  }

  private def parseIdLine(input: Input) = {
    if (input.isLeft)
      IdPattern
        .findFirstMatchIn(input.left.get)
        .map(
          m => Id(toDepth(m.group(1)), Tag(m.group(4)), m.group(3),
            toMultiplicity(m.group(5), m.group(6)))) match {
            case Some(e) => Right(e)
            case None => input
          }
    else
      input
  }

  private def parseIdReferenceLine(input: Input) = {
    if (input.isLeft)
      IdReferencePattern
        .findFirstMatchIn(input.left.get)
        .map(
          m => IdReference(
            toDepth(m.group(1)),
            Tag(m.group(3)),
            m.group(4),
            toMultiplicity(m.group(5), m.group(6)))) match {
            case Some(e) => Right(e)
            case None => input
          }
    else
      input
  }

  private def parseDefinitionReferenceLine(input: Input) = {
    if (input.isLeft)
      DefinitionReferencePattern.findFirstMatchIn(input.left.get).map(
        m => DefinitionReference(toDepth(m.group(1)), m.group(3),
          toMultiplicity(m.group(4), m.group(5)))) match {
          case Some(e) => Right(e)
          case None => input
        }
    else
      input
  }

  private def toDepth(s: String) = {
    if (s == "0")
      ZeroDepth
    else if (s == "n")
      AnyDepth
    else
      RelativeDepth(s.substring(1).toInt)
  }

  //1=relative level,4=def ref, 6=tag, 9=xref,10=name, 11=min,12=max
  def parse(line: String): Option[Element] = {
    val input: Input = Left(line)
    val parsers = List(
      parseDefinitionLine(_),
      parseValueLine(_),
      parseIdLine(_),
      parseIdReferenceLine(_),
      parseDefinitionReferenceLine(_))

    parsers
      .reduce(_ compose _)(input)
      .right.toOption
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