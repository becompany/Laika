package laika.parse.kramdown

import laika.parse.helper.{DefaultParserHelpers, ParseResultHelpers}
import laika.parse.markdown.InlineParsers
import laika.tree.Elements._
import laika.tree.helper.ModelBuilder
import org.scalatest.{FlatSpec, Matchers}

class TableParsersSpec extends FlatSpec
  with Matchers
  with BlockParsers
  with InlineParsers
  with ParseResultHelpers
  with DefaultParserHelpers[RootElement]
  with ModelBuilder {

  val defaultParser: Parser[RootElement] = rootElement

  "The table parser" should "parse tables" in {
    val input = """|| a | b |
                   ||---
                   || c | d |
                   ||---
                   || e | f |
                   ||===
                   || g | h |
                   |""".stripMargin

    Parsing (input) should produce (root(Table(
      TableHead(Seq(
        Row(Seq(Cell(HeadCell, Seq(p("a"))), Cell(HeadCell, Seq(p("b")))))
      )),
      TableBody(Seq(
        Row(Seq(Cell(BodyCell, Seq(p("c"))), Cell(BodyCell, Seq(p("d"))))),
        Row(Seq(Cell(BodyCell, Seq(p("e"))), Cell(BodyCell, Seq(p("f")))))
      )),
      TableFoot(Seq(
        Row(Seq(Cell(BodyCell, Seq(p("g"))), Cell(BodyCell, Seq(p("h")))))
      ))
    )))
  }

}