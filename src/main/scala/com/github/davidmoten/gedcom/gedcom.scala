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

sealed trait Node {
  val level: Int
  val children: List[Child]
  def format: String
}

object Node {
  def add(children: List[Child], record: Record): List[Child] =
    Child(record, List()) :: children
}

case class Child(record: Record, children: List[Child]) extends Node {
  val level = record.level

  def add(r: Record): Child = {
    if (r.level == level + 1)
      Child(record, Node.add(children, r))
    else
      new Child(record, children.head.add(r) :: children.tail)
  }

  val indent = "  " * level

  def format: String = indent +
    ("Child(" + record + ")" ::
      children.reverse.map(_.format)).mkString("\n")
}

case class Root(children: List[Child]) extends Node {
  val level = -1

  def add(r: Record): Root = {
    if (r.level == level + 1)
      Root(Node.add(children, r))
    else
      new Root(children.head.add(r) :: children.tail)
  }

  def format: String = "Root\n" +
    children.reverse.map(_.format).mkString("\n")
}

class GedcomParser {

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