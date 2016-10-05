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

package laika.parse.kramdown

import laika.directive.Directives.{Blocks, Spans}
import laika.directive.StandardDirectives
import laika.factory.ParserFactory
import laika.io.Input
import laika.parse.markdown.InlineParsers
import laika.parse.markdown.html.HTMLParsers
import laika.rewrite.TreeUtil
import laika.template.TemplateParsers
import laika.tree.Documents.Document
import laika.tree.Elements.{Block, Header, Id}

class Kramdown private(
    blockDirectives: List[Blocks.Directive],
    spanDirectives: List[Spans.Directive],
    verbatimHTML: Boolean,
    isStrict: Boolean) extends ParserFactory {

  val fileSuffixes: Set[String] = Set("md","markdown")

  val rewriteRules = Seq()

  def withBlockDirectives (directives: Blocks.Directive*): Kramdown =
    new Kramdown(blockDirectives ++ directives, spanDirectives, verbatimHTML, isStrict)

  def withSpanDirectives (directives: Spans.Directive*): Kramdown =
    new Kramdown(blockDirectives, spanDirectives ++ directives, verbatimHTML, isStrict)

  def withVerbatimHTML: Kramdown = new Kramdown(blockDirectives, spanDirectives, true, isStrict)

  def strict: Kramdown = new Kramdown(blockDirectives, spanDirectives, verbatimHTML, true)

  private lazy val parser: BlockParsers with InlineParsers = {
    trait ExtendedParsers extends TemplateParsers.MarkupBlocks with TemplateParsers.MarkupSpans with StandardDirectives {
      lazy val blockDirectiveMap = Blocks.toMap(stdBlockDirectives) ++ Blocks.toMap(blockDirectives)
      lazy val spanDirectiveMap = Spans.toMap(stdSpanDirectives) ++ Spans.toMap(spanDirectives)
      def getBlockDirective (name: String) = blockDirectiveMap.get(name)
      def getSpanDirective (name: String) = spanDirectiveMap.get(name)

      override def blockList (parser: => Parser[Block]): Parser[List[Block]] = super.blockList(parser) ^^ {
        _ map { case h: Header =>
            h.copy(options = h.options + Id(TreeUtil.extractText(h.content).replaceAll("[\n ]+", " ").toLowerCase))
          case other => other
        }
      }
    }
    if (verbatimHTML && !isStrict) new BlockParsers with InlineParsers with ExtendedParsers with HTMLParsers
    else if (verbatimHTML)         new BlockParsers with InlineParsers with HTMLParsers
    else if (!isStrict)            new BlockParsers with InlineParsers with ExtendedParsers
    else                           new BlockParsers with InlineParsers
  }

  /** The actual parser function, fully parsing the specified input and
   *  returning a document tree.
   */
  val newParser: Input => Document = (input: Input) => parser.parseDocument(input.asParserInput, input.path)

}

/** The default Markdown parser configuration, with verbatim HTML elements disabled.
 *
 *  @author Jens Halm
 */
object Kramdown extends Kramdown(Nil,Nil,false,false)
