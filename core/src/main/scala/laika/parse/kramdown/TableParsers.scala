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

import laika.parse.markdown.InlineParsers
import laika.tree.Elements._

trait TableParsers extends laika.parse.BlockParsers { self: InlineParsers =>
   
  /**
   * Parses a table.
   * See [[http://kramdown.gettalong.org/quickref.html#tables]].
   */
  def table: Parser[Table] = {

    sealed trait RowElement
    trait SepElement

    case class CellRow(cells: List[Block]) extends RowElement
    object BodySep extends RowElement with SepElement
    object FootSep extends RowElement with SepElement
    
    val colSep = anyOf('|') take 1

    def simpleRowSep(c: Char) = anyOf(c).min(1)
    def columnsRowSep(c: Char) = (ws.? ~ simpleRowSep(c) ~ ws.? ~ colSep).*

    def rowSep(c: Char) = (simpleRowSep(c) | columnsRowSep(c)) ~ eol

    val tBodySep = rowSep('-') ^^^ BodySep
    val tFootSep = rowSep('=') ^^^ FootSep

    val cell = anyUntil(colSep | eol).min(1) ^^ { text =>
      Paragraph(parseInline(text.trim))
    }

    val cellRow = cell.+ <~ eol ^^ CellRow

    val row = colSep ~> (tBodySep | tFootSep | cellRow)

    def splitRows(rows: List[RowElement], sep: SepElement = BodySep): List[(List[CellRow], SepElement)] = {
      val cellRows = rows.takeWhile(_.isInstanceOf[CellRow]).map(_.asInstanceOf[CellRow])
      val num = cellRows.size
      val tail = if (num == rows.size) {
         Nil
      } else {
        splitRows(rows.drop(num + 1), rows(num).asInstanceOf[SepElement])
      }
      (cellRows, sep) :: tail
    }

    row.+ <~ guard(anyBut('|')) ^^ { rowList =>
      val parts = splitRows(rowList).groupBy(_._2).mapValues(_.flatMap(_._1).map(_.cells))
      val nonFootBlocks = parts.getOrElse(BodySep, Nil)
      val footBlocks = parts.getOrElse(FootSep, Nil)

      val split = if (nonFootBlocks.size > 1) 1 else 0
      val (headBlocks, bodyBlocks) = nonFootBlocks.splitAt(split)

      Table(
        TableHead(headBlocks.map(cells => Row(cells.map(cell => Cell(HeadCell, Seq(cell)))))),
        TableBody(bodyBlocks.map(cells => Row(cells.map(cell => Cell(BodyCell, Seq(cell)))))),
        TableFoot(footBlocks.map(cells => Row(cells.map(cell => Cell(BodyCell, Seq(cell))))))
      )
    }

  }
  
}
