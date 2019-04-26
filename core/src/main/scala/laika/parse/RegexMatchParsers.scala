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