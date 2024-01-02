/*
    Gauss elimination algorithm, to reduce a matrix to row-echelon form.
    Copyright (C) 2023-2024  Dustin Thomas

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

package dev.cptlobster.scatrix
package algorithms

import scala.annotation.tailrec

class GaussElimination (val mat: LegacyMatrix, val pivot: (Int, Int) = (0, 0)) {
  val px: Int = pivot._1
  val py: Int = pivot._2
  val pivot_col: List[Float] = mat.col(py)
  val pivot_row: List[Float] = mat.row(px)
  val pivot_val: Float = mat.cell(px, py)
  def reduce: GaussElimination = {
    val reductions = pivot_col.drop(px).tail.map(n => n / pivot_val)
    printf(reductions.toString)
    @tailrec def get_ops(l: List[Float], row: Int, ero: ElementaryOps): ElementaryOps = {
      l match
        case Nil => ero
        case ::(h, t) => get_ops(t, row + 1, ero.add(px, row, -h))
    }
    val adds = get_ops(reductions, px + 1, ElementaryOps(mat.rows))
    val ops = if pivot_val != 1 then adds.mult(px, 1 / pivot_val) else adds
    printf(ops.toString)
    GaussElimination(ops.mat * mat, (px + 1, py + 1))
  }
}
