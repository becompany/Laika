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

import laika.ast._
import laika.parse.{Parser, RegexMatchParsers}
import laika.markdown.BlockParsers._
import laika.markdown.InlineParsers._
import laika.parse.text.WhitespacePreprocessor
import laika.parse.text.TextParsers._

trait BlockParsers extends TableParsers with RegexMatchParsers {

  private val processWS = new WhitespacePreprocessor
  private val language = anyIn('a' to 'z', '-') min 1

  /** Parses a literal block fence.
    */
  val literalBlockFence: Parser[Any] = anyOf('~') min 3

  /** Parses a fenced literal block.
    */
  def fencedLiteralBlock: Parser[Block] =
    fencedBlock(literalBlockFence) ^^ {
      case ~(languageOpt, lines) =>
        val code = lines.map(processWS).mkString("\n")
        languageOpt match {
          case Some(lang) => CodeBlock(lang, Seq(Text(code)))
          case None => LiteralBlock(code)
        }
    }

  /**
    * Parses a fenced code block with optional language.
    * @param fence The parser for the fence.
    * @return A parser returning an optional language and the code lines.
    */
  def fencedBlock(fence: => Parser[Any]): Parser[~[Option[String], List[String]]] = {
    val firstLine = fence ~> (ws ~> language).? <~ eol
    lazy val codeLine = not(fence) ~> restOfLine
    lazy val lastLine = fence <~ eol
    firstLine ~ codeLine.* <~ lastLine
  }

  private val cssRegex = """\{\:((?:\s*\.?[a-zA-Z\-]+)+)\}""".r

  def inlineAttributes: Parser[List[String]] =
    regexMatch(cssRegex) <~ eol ^^ (_.group(1).split("""\s*\.""").drop(1).toList)

  override def standardMarkdownBlock: Parser[Block] =
    atxHeader | setextHeader | (insignificantSpaces ~>
      (table | fencedLiteralBlock | literalBlock | quotedBlock | rule | bulletList | enumList))

  override protected def prepareBlockParsers(nested: Boolean): List[Parser[Block]] = {
    super.prepareBlockParsers(nested) map { parser =>
      inlineAttributes.? ~ parser ^^ {
        case ~(None, block) => block
        case ~(Some(classes), block) =>
          val styles = Styles(classes :_*)
          block match {
            case p: Paragraph => p.copy(options = p.options + styles)
            case p: CodeBlock => p.copy(options = p.options + styles)
            case p: LiteralBlock => p.copy(options = p.options + styles)
            case p: BulletList => p.copy(options = p.options + styles)
            case p: EnumList => p.copy(options = p.options + styles)
            case p: Table => p.copy(options = p.options + styles)
            case p => p
          }
      }
    }

  }

}