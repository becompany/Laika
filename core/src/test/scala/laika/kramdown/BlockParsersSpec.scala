/*
 * Copyright 2013-2016 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package laika.kramdown

import laika.parse.helper.{DefaultParserHelpers, ParseResultHelpers}
import laika.parse.markdown.InlineParsers
import laika.tree.Elements._
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

  "The block parser" should "parse CSS classes in paragraphs" in {
    val input =
      """{: .foo .bar.baz}
        |text
      """.stripMargin
    Parsing (input) should produce (root(Paragraph(Seq(Text("text")), Styles("foo", "bar", "baz"))))
  }

  it should "parse CSS classes in code blocks" in {
    val input =
      """{: .foo .bar.baz}
        |~~~~ scala
        |code
        |~~~~
      """.stripMargin
    Parsing (input) should produce (root(CodeBlock("scala", Seq(Text("code")), Styles("foo", "bar", "baz"))))
  }

}