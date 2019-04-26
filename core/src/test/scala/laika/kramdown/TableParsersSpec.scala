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
import laika.markdown.InlineParsers
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
    val input = """foo
                  |
                  || a | b |
                  ||---
                  || c | d |
                  || - | - |
                  || e | f |
                  ||===
                  || g | h |
                  |
                  |bar""".stripMargin

    Parsing (input) should produce (root(p("foo"),
      Table(
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
      ),
      p("bar")
    ))
  }

}