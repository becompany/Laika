package laika.parse.kramdown

import laika.parse.markdown.InlineParsers
import laika.parse.util.WhitespacePreprocessor
import laika.tree.Elements.{Block, CodeBlock, LiteralBlock, Text}

trait BlockParsers extends laika.parse.markdown.BlockParsers {

  self: InlineParsers =>

  private val processWS = new WhitespacePreprocessor
  private val language = anyIn('a' to 'z', '-') min 1

  /** Parses a literal block fence.
    */
  val literalBlockFence: Parser[Any] = anyOf('~') take 4

  /** Parses a fenced literal block.
    */
  override def literalBlock: Parser[Block] =
    super.literalBlock | (fencedBlock(literalBlockFence) ^^ {
      case ~(languageOpt, lines) =>
        val code = lines.map(processWS).mkString("\n")
        languageOpt match {
          case Some(lang) => CodeBlock(lang, Seq(Text(code)))
          case None => LiteralBlock(code)
        }
    })

  /**
    * Parses a fenced code block with optional language.
    * @param fence The parser for the fence.
    * @return A parser returning an optional language and the code lines.
    */
  def fencedBlock(fence: => Parser[Any]): Parser[~[Option[String], List[String]]] = {
    val firstLine = fence ~> (ws ~> language).? <~ eol
    lazy val codeLine = guard(not(fence)) ~> restOfLine
    lazy val lastLine = fence <~ eol
    firstLine ~ codeLine.* <~ lastLine
  }

}
