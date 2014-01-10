package com.github.davidmoten.gedcom.grammar

trait Element
sealed trait Depth
sealed trait Multiplicity
case class Tag(value: String)
case class Value(depth: Depth, tag: Tag, name: String, mult: Multiplicity) extends Element
case class Definition(name: String, node: Node)
case class Grammar(root: Definition, definitions: List[Definition])
case class Node(level: Int, element: Element, children: Node)

private object Line {
  import java.util.regex._

  private val multPatternEnding = "\\s*{(\\d:(\\d|M))}\\s*$"

  private val valuePattern = Pattern.compile(
    "^\\s*(0|n|(\\+\\d+))\\s+(\\w+)\\s+<(\\w+)>" +
      multPatternEnding, Pattern.DOTALL)

  private val idPattern = Pattern.compile("^\\s*(0|n|(\\+\\d+))\\s+(\\w+)" +
    multPatternEnding, Pattern.DOTALL)
  private val definitionPattern = Pattern.compile(
    "^\\s*(\\w+): =\\s*$", Pattern.DOTALL)
  private val definitionReferencePatter = Pattern.compile(
    "^\\s*(0|n|(\\+\\d+))\\s+<<(\\w+)>>" +
      multPatternEnding, Pattern.DOTALL)

  //1=relative level,4=def ref, 6=tag, 9=xref,10=name, 11=min,12=max
  def parse(line: String) = {
    val m = valuePattern.matcher(line)
    if (!m.find)
      None
    else
      Some(line)
  }
}

class Parser(is: java.io.InputStream) {

  def parse = {
    val empty:(List[List[String]],List[String])= (List(), List())
    io.Source.fromInputStream(is)
      .getLines
      .filter(_.trim.length > 0)
      .foldLeft(empty)((g,line) => g)
  }

}