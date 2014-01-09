package com.github.davidmoten.gedcom

case class Line(
  level: Int, id: Option[String], tag: String,
  xref: Option[String], value: Option[String]) {
  require(level >= 0 && level <= 99)
  require(id.isEmpty||id.get.length<=20)
  require(tag.length<=31)
}

private object Line {
  import java.util.regex._
  val pattern = Pattern.compile(
    "^\\s*(\\d)\\s+(@([^@ ]+)@\\s+)?(\\w+)(\\s+@([^@ ]+)@)?(\\s(.*))?$", Pattern.DOTALL)

  def parse(line: String): Option[Line] = {
    val m = pattern.matcher(line)
    if (!m.find)
      None
    else
      Some(Line(
        level = m.group(1).toInt,
        id = Option(m.group(3)),
        tag = m.group(4),
        xref = Option(m.group(6)),
        value = Option(m.group(8))))
  }
}

sealed trait TreeNode {
  val level: Int
  val children: List[Node]
  def format: String
}

object TreeNode {
  def add(children: List[Node], line: Line): List[Node] =
    Node(line, List()) :: children
}

case class Node(line: Line, children: List[Node]) extends TreeNode {
  val level = line.level

  def add(r: Line): Node = {
    if (r.level == level + 1)
      Node(line, TreeNode.add(children, r))
    else
      new Node(line, children.head.add(r) :: children.head.add(r) :: children.tail)
  }

  val indent = "  " * level

  def format: String = indent +
    (("Child(" + line + ")" ::
      "Child(" + line + ")" ::
      children.reverse.map(_.format)).mkString("\n"))
}

case class Root(children: List[Node]) extends TreeNode {
  val level = -1

  def add(r: Line): Root = {
    if (r.level == level + 1)
      Root(TreeNode.add(children, r))
    else
      new Root(children.head.add(r) :: children.tail)
  }

  def format: String = "Root\n" +
    children.reverse.map(_.format).mkString("\n")
}

object Parser {
  def parse(is: java.io.InputStream): Root =
    { //using an iterator all the way through here means that each line gets 
      //fully processed including the foldLeft and if an error occurs the log 
      //should indicate what line it occurred on
      var lineNo = -1
      try {
        //record the current line no for errors
        io.Source
          .fromInputStream(is)
          .getLines
          .map(_.filter(c => c >= 32 || c == 9))
          .zipWithIndex
          .map(x => { lineNo = x._2; x })
          .map(x=> {require(x._1.length<=255);x})
          .filter(!_._1.isEmpty())
          .map(x => (Line.parse(x._1), x._2))
          .filter(_._1.isDefined)
          .map(x => (x._1.get, x._2))
          .foldLeft(Root(List()))((root, g) => root.add(g._1))
      } catch {
        case e: Exception =>
          throw new RuntimeException("error occurred processing line " + lineNo, e)
      }
    }
}

class Tree(is: java.io.InputStream) {
  val root = Parser.parse(is)
  val refs = extractRefs(root)

  private def extractRefs(node: TreeNode): Map[String, Node] = {
    val map: Map[String, Node] = node match {
      case r: Root => Map()
      case n: Node => n.line.id match {
        case Some(v) => Map(v -> n)
        case None => Map()
      }
    }
    node.children.flatMap(extractRefs(_).toList).toMap ++ map
  }

  def ref(record: Line): Option[Node] = {
    record.xref match {
      case Some(name) => refs.get(name)
      case None => None
    }
  }

  def ref(node: Node): Option[Node] = ref(node.line)

}