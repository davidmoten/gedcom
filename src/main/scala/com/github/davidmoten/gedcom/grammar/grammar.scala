package com.github.davidmoten.gedcom.grammar

sealed trait Element {
  def depth: Depth
}

sealed trait NodeElement {
  def depth: Depth
}

sealed trait Depth
case object AnyDepth extends Depth
case object MinusOneDepth extends Depth
case object ZeroDepth extends Depth
case class RelativeDepth(depth: Int) extends Depth {
  require(depth >= 1)
}
sealed trait MultiplicityBound
case class Specific(value: Integer) extends MultiplicityBound {
  require(value >= 0)
}
case object Unbounded extends MultiplicityBound
case class Multiplicity(min: MultiplicityBound, max: MultiplicityBound)
case class Tag(value: String)
case class Value(depth: Depth, tag: Tag,
  name: String, mult: Multiplicity) extends Element with NodeElement
case class TagOnly(depth: Depth, tag: Tag,
  mult: Multiplicity) extends Element with NodeElement
case class Id(depth: Depth, tag: Tag, id: String,
  mult: Multiplicity) extends Element with NodeElement
case class IdWithValue(depth: Depth, tag: Tag, id: String, value: String,
  mult: Multiplicity) extends Element with NodeElement
case class IdReference(depth: Depth, tag: Tag, id: String,
  mult: Multiplicity) extends Element with NodeElement
case class DefinitionReference(depth: Depth, name: String, mult: Multiplicity)
  extends Element with NodeElement

case class Definition(name: String, children: List[Node]) extends NodeElement {
  import Util._

  val depth = MinusOneDepth

  def add(element: NodeElement): Definition = {
    element.depth match {
      case ZeroDepth | AnyDepth => Definition(name, Node(element, List()) :: children)
      case MinusOneDepth => Definition(name, Node(element, List()) :: children)
      case _ => Definition(name, children.head.add(element) :: children.tail)
    }
  }
}

case class DefinitionLine(name: String) extends Element {
  val depth = ZeroDepth
}
sealed trait ElementWithoutDepth extends Element {
  def depth = MinusOneDepth
}

case object OpenBracket extends ElementWithoutDepth with NodeElement
case object CloseBracket extends ElementWithoutDepth with NodeElement
case object Or extends ElementWithoutDepth with NodeElement

object Util {
  def unexpected(s: String) = throw new RuntimeException(s)
  def unexpected = throw new RuntimeException
}

case class Node(element: NodeElement, children: List[Node]) {
  import Util._

  def add(nextElement: NodeElement): Node = {
    println("adding " + nextElement + " to " + this)
    lazy val nodeWithNewChild = Node(element, Node(nextElement, List()) :: children)
    lazy val addedToFirstChild = Node(element, children.head.add(nextElement) :: children.tail)
    nextElement.depth match {
      case AnyDepth => nodeWithNewChild
      case ZeroDepth => element match {
        case Definition(_, _) => nodeWithNewChild
        case _ => unexpected
      }
      case RelativeDepth(depth) =>
        element.depth match {
          case AnyDepth | ZeroDepth if (depth == 1) =>
            nodeWithNewChild
          case AnyDepth | ZeroDepth if (depth != 1 && children.isEmpty) =>
            unexpected
          case AnyDepth | ZeroDepth if (depth != 1 && !children.isEmpty) =>
            addedToFirstChild
          case RelativeDepth(d) if (depth == d + 1) =>
            nodeWithNewChild
          case RelativeDepth(d) if (depth > d + 1) =>
            addedToFirstChild
          case RelativeDepth(d) if (depth < d + 1) =>
            unexpected
        }
    }
  }
}

case class Grammar(definitions: List[Definition]) {
  def root = definitions(0)
}

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

    val TagOnlyPattern = (
      depthPattern +
      "(\\w+)\\s" +
      multPatternEnding).r

    val IdPattern = (
      depthPattern +
      "@([^@]+)@\\s+(\\w+)" +
      multPatternEnding).r

    val IdPatternWithValue = (
      depthPattern +
      "@([^@]+)@\\s+(\\w+)\\s+<(\\w+)>" +
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

    val OpenBracketPattern = "^\\s*\\[\\s*$".r
    val CloseBracketPattern = "^\\s*\\]\\s*$".r
    val OrPattern = "^\\s*\\|\\s*$".r
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
    import Util._
    line match {
      case ValuePattern(depth, tag, value, min, max) =>
        Some(Value(toDepth(depth), Tag(tag), value, toMultiplicity(min, max)))
      case DefinitionPattern(name) =>
        Some(DefinitionLine(name))
      case IdPattern(depth, id, tag, min, max) =>
        Some(Id(toDepth(depth), Tag(tag), id, toMultiplicity(min, max)))
      case IdPatternWithValue(depth, id, tag, value, min, max) =>
        Some(IdWithValue(toDepth(depth), Tag(tag), id, value, toMultiplicity(min, max)))
      case IdReferencePattern(depth, tag, id, min, max) =>
        Some(IdReference(toDepth(depth), Tag(tag), id, toMultiplicity(min, max)))
      case DefinitionReferencePattern(depth, name, min, max) =>
        Some(DefinitionReference(toDepth(depth), name, toMultiplicity(min, max)))
      case TagOnlyPattern(depth, tag, min, max) =>
        Some(TagOnly(toDepth(depth), Tag(tag), toMultiplicity(min, max)))
      case OpenBracketPattern() => Some(OpenBracket)
      case CloseBracketPattern() => Some(CloseBracket)
      case OrPattern() => Some(Or)
      case _ => unexpected(line)
    }
  }
}

class Parser(is: java.io.InputStream) {

  import Util._
  def toSchemaElement(element: NodeElement) =
    element match {
      case x: Definition => <xs:complexType name="{x.name}"></xs:complexType>

    }

  def parseLines = {
    io.Source.fromInputStream(is)
      .getLines
      .filter(_.trim.length > 0)
      .flatMap(Grammar.parse(_))
  }

  def parse = {
    val g = Grammar(List())
    parseLines
      .foldLeft(g)((g, element) =>
        element match {
          case x: DefinitionLine => Grammar(Definition(x.name, List()) :: g.definitions)
          case x: NodeElement => Grammar(g.definitions.head.add(x) :: g.definitions.tail)
          case _ => unexpected(element.toString)
        })
  }

}