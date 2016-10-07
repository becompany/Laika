package laika.parse

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

trait RegexMatchParsers {
  this: RegexParsers =>

  /**
    * A parser that matches a regex string and returns the Match.
    * http://stackoverflow.com/questions/1815716/accessing-scala-parser-regular-expression-match-data
    */
  def regexMatch(r: Regex): Parser[Regex.Match] = new Parser[Regex.Match] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      r findPrefixMatchOf source.subSequence(start, source.length) match {
        case Some(matched) =>
          Success(matched, in.drop(start + matched.end - offset))
        case None =>
          Failure(s"string matching regex '$r' expected but '${in.first}' found", in.drop(start - offset))
      }
    }
  }

}
