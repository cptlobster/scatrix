/*
    Chainable Elementary Row Operation class
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

class ElementaryOps(val mat: Matrix) {
  override def toString: String = mat.toString

  /** Perform a multiplication Elementary Row Operation; multiply one row by a scalar
   *
   * {{{
   * [1 0 0] 5r2 => r2 [1 0 0]
   * [0 1 0] --------> [0 5 0]
   * [0 0 1]           [0 0 1]
   * }}}
   */
  def mult(row: Int, scalar: Double): ElementaryOps = {
    val op = Matrix((for (i <- 0 until mat.rows) yield {
      (for (j <- 0 until mat.cols) yield {
        if i == j then if i == row then scalar else 1 else 0
      }).toList
    }).toList)
    ElementaryOps(mat * op)
  }

  /** Perform a swap Elementary Row Operation; swap two rows
   *
   * {{{
   * [1 0 0] r1 <=> r2 [0 1 0]
   * [0 1 0] --------> [1 0 0]
   * [0 0 1]           [0 0 1]
   * }}}
   */
  def swap(r1: Int, r2: Int): ElementaryOps = {
    val op = Matrix((for (i <- 0 until mat.rows) yield {
      (for (j <- 0 until mat.cols) yield {
        if j == r1 && i == r2 then 1d
        else if i == r1 && j == r2 then 1d
        else if i == j then 1d
        else 0d
      }).toList
    }).toList)
    ElementaryOps(mat * op)
  }

  /** Perform an addition Elementary Row Operation; Add one row to another
   *
   * {{{
   * [1 0 0] r1 + 5r2 => r1 [1 5 0]
   * [0 1 0] -------------> [0 1 0]
   * [0 0 1]                [0 0 1]
   * }}}
   */
  def add(rs: Int, rt: Int, scalar: Double = 1f): ElementaryOps = {
    val op = Matrix((for (i <- 0 until mat.rows) yield {
      (for (j <- 0 until mat.cols) yield {
        if i == rt && j == rs then scalar
        else if i == j then 1d
        else 0d
      }).toList
    }).toList)
    ElementaryOps(mat * op)
  }
}

object ElementaryOps {
  def apply(mat: Matrix): ElementaryOps = new ElementaryOps(mat)
  def apply(size: Int): ElementaryOps = new ElementaryOps(Matrix.identity(size))
}