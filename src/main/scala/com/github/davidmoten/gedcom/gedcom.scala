package com.github.davidmoten.gedcom

case class Record(
  level: Int, id: Option[String], tag: String,
  xref: Option[String], value: Option[String])

private object Record {
  import java.util.regex._
  val pattern = Pattern.compile(
    "^\\s*(\\d)\\s+(@([^@ ]+)@\\s+)?(\\w+)(\\s+@([^@ ]+)@)?(\\s(.*))?$", Pattern.DOTALL)

  def parse(line: String): Option[Record] = {
    val m = pattern.matcher(line)
    if (!m.find)
      None
    else
      Some(Record(
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
  def add(children: List[Node], record: Record): List[Node] =
    Node(record, List()) :: children
}

case class Node(record: Record, children: List[Node]) extends TreeNode {
  val level = record.level

  def add(r: Record): Node = {
    if (r.level == level + 1)
      Node(record, TreeNode.add(children, r))
    else
      new Node(record, children.head.add(r) :: children.head.add(r) :: children.tail)
  }

  val indent = "  " * level

  def format: String = indent +
    (("Child(" + record + ")" ::
      "Child(" + record + ")" ::
      children.reverse.map(_.format)).mkString("\n"))
}

case class Root(children: List[Node]) extends TreeNode {
  val level = -1

  def add(r: Record): Root = {
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
    io.Source
      .fromInputStream(is)
      .getLines
      .map(_.filter(c => c >= 32 || c == 9))
      .zipWithIndex
      .filter(!_._1.isEmpty())
      .map(x => (Record.parse(x._1), x._2))
      .filter(_._1.isDefined)
      .map(x => (x._1.get, x._2))
      .foldLeft(Root(List()))((root, g) => root.add(g._1))
  //      .map(x => { println(x); x })

}

class Parser(is: java.io.InputStream) {
  val root = Parser.parse(is)
  val refs = extractRefs(root)
  
  private def extractRefs(node: TreeNode): Map[String, Node] = {
    val map:Map[String,Node] = node match {
      case r: Root => Map()
      case n: Node => n.record.id match {
        case Some(v) => Map(v -> n)
        case None => Map()
      }
    }
    node.children.flatMap(extractRefs(_).toList).toMap ++ map 
  }
  
  def ref(record:Record):Option[Node] = {
    record.xref match {
      case Some(name) => refs.get(name)
      case None => None
    }
  }
  
  def ref(node:Node):Option[Node] = ref(node.record)
  
}