package laika.parse.kramdown

import laika.parse.helper.{DefaultParserHelpers, ParseResultHelpers}
import laika.parse.markdown.InlineParsers
import laika.tree.Elements.RootElement
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
    val input = """+---+---+
                  || a | b |
                  |+---+---+
                  || c | d |
                  |+---+---+""".stripMargin
    Parsing (input) should produce (root(table(strrow("a","b"), strrow("c","d"))))
  }

}