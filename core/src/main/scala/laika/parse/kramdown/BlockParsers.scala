package laika.parse.kramdown

import laika.parse.RegexMatchParsers
import laika.parse.markdown.InlineParsers
import laika.parse.util.WhitespacePreprocessor
import laika.tree.Elements._

trait BlockParsers extends laika.parse.markdown.BlockParsers with RegexMatchParsers {

  self: InlineParsers =>

  private val processWS = new WhitespacePreprocessor
  private val language = anyIn('a' to 'z', '-') min 1

  /** Parses a literal block fence.
    */
  val literalBlockFence: Parser[Any] = anyOf('~') min 3

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
    lazy val codeLine = not(fence) ~> restOfLine
    lazy val lastLine = fence <~ eol
    firstLine ~ codeLine.* <~ lastLine
  }

  private val cssRegex = """\{\:((?:\s*\.?[a-zA-Z\-]+)+)\}""".r

  def inlineAttributes: Parser[List[String]] =
    regexMatch(cssRegex) <~ eol ^^ (_.group(1).split("""\s*\.""").drop(1).toList)

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
            case p => p
          }
      }
    }

  }

}
