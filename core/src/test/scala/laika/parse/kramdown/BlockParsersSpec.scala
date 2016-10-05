package laika.parse.kramdown

import laika.parse.helper.{DefaultParserHelpers, ParseResultHelpers}
import laika.parse.markdown.InlineParsers
import laika.tree.Elements.{RootElement, Text}
import laika.tree.helper.ModelBuilder
import org.scalatest.{FlatSpec, Matchers}

class BlockParsersSpec extends FlatSpec
  with Matchers
  with BlockParsers
  with InlineParsers
  with ParseResultHelpers
  with DefaultParserHelpers[RootElement]
  with ModelBuilder {

  val defaultParser: Parser[RootElement] = rootElement

  "The fenced code block parser" should "parse paragraphs within a fence as a code block" in {
    val input = """text
                  |
                  |~~~~
                  |code
                  |~~~~
                  |
                  |text""".stripMargin
    Parsing (input) should produce (root(p("text"), litBlock("code"), p("text")))
  }

  it should "parse code blocks with language" in {
    val input = """text
                  |
                  |~~~~ scala
                  |code
                  |~~~~
                  |
                  |text""".stripMargin
    Parsing (input) should produce (root(p("text"), codeBlock("scala", Seq(Text("code"))), p("text")))
  }

}
