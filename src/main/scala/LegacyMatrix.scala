/*
    Primary Matrix class and objects. (DEPRECATED)
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

import exceptions.{InvalidDimensionException, MultiplySizeMismatchException, NotSquareMatrixException, SizeException}

import scala.annotation.{tailrec, targetName}

/** 2D Matrix class.
 *
 * == Overview ==
 *
 * The Matrix class contains 2D array of elements.
 *
 */
class LegacyMatrix(val contents: List[List[Double]]) {
  /* Display */
  override def toString: String = (for (i <- contents) yield { i.mkString("[", " ", "]") }).mkString("\n")
  def asLatex: String = (for (i <- contents) yield { i.mkString("", " & ", "") }).mkString("\\begin{bmatrix}\n", "\n", "\\end{bmatrix}")
  def asMarkdown: String = (for (i <- contents) yield { i.mkString("| ", " | ", " |") }).mkString("\n")

  /* Sizing */
  /** Amount of rows in matrix */
  val rows: Int = contents.size
  /** Amount of columns in matrix */
  val cols: Int = contents.head.size
  /** Size stored as a tuple, in order of rows, then columns */
  val size: (Int, Int) = (rows, cols)

  /* Conditionals */
  /** Check if all rows are same length. */
  def valid_size: Boolean = {
    val sizes = contents.map(l => l.size)
    sizes.forall(n => n == sizes.head)
  }
  /** Check if this matrix is the same size as another matrix. */
  def same_size(other: LegacyMatrix): Boolean = size == other.size
  /** Check if this matrix is the correct size to multiply with another matrix. */
  def can_multiply(other: LegacyMatrix): Boolean = size._2 == other.size._1
  /** Check if matrix is square (amount of rows equals amount of columns). */
  def square: Boolean = rows == cols
  /** Check if matrices are equal */
  @targetName("==")
  def ==(other: LegacyMatrix): Boolean = contents == other.contents
  /** Check if matrices are inverse of each other */
  def is_inverse(other: LegacyMatrix): Boolean = this * other == LegacyMatrix.identity(rows)

  /* Selection */
  /** Select a single row from the matrix. **/
  def row(idx: Int): List[Double] = contents.drop(idx).head
  /** Select a single column from the matrix. */
  def col(idy: Int): List[Double] = for (r <- contents) yield { r.drop(idy).head }
  /** Select a single cell value from the matrix */
  def cell(idx: Int, idy: Int): Double = row(idx).drop(idy).head
  def main_diag: List[Double] = {
    if square then (for (i <- 0 until cols) yield {
      cell(i, i)
    }).toList
    else throw NotSquareMatrixException()
  }

  /* Append */
  def add_row(other: List[Double]): LegacyMatrix = if other.size == cols then LegacyMatrix(contents :+ other) else throw SizeException()
  def add_col(other: List[Double]): LegacyMatrix =
    @tailrec def append_col(l: List[Double], m: List[List[Double]], acc: List[List[Double]]): LegacyMatrix = l match
      case Nil => LegacyMatrix(acc)
      case ::(h, t) => append_col(t, m.tail, acc :+ (m.head :+ h))
    if other.size == rows then append_col(other, contents, List[List[Double]]()) else throw SizeException()
  def map(f: Double => Double): LegacyMatrix = LegacyMatrix(for (i <- contents) yield { i.map(f) })

  def take_row(idx: Int): LegacyMatrix = LegacyMatrix(contents.take(idx) ++ contents.drop(idx).tail)
  def take_col(idy: Int): LegacyMatrix = LegacyMatrix(for (i <- contents) yield { i.take(idy) ++ i.drop(idy).tail })

  /* Determinant */
  /** Get the minor matrix for a cell; That is, the matrix without the specified row and column.
   * {{{
   * [1 2 3]
   * [4 5 6] minor(1, 1) = [5 6]
   * [7 8 9]               [8 9]
   * }}}
   */
  def minor(idx: Int, idy: Int): LegacyMatrix = take_row(idx).take_col(idy)

  /**
   * Get the determinant of a matrix.
   * {{{
   *   |1 2 3|
   *   |4 5 6| = 0
   *   |7 8 9|
   * }}}
   * @return
   */
  def det: Double = {
    def det_inner(mat: LegacyMatrix): Double = mat.size._1 match
      case 2 => (mat.cell(0, 0) * mat.cell(1, 1)) - (mat.cell(1, 0) * mat.cell(0, 1))
      case _ => (for (i <- 0 until cols) yield {
        (-1 * (i % 2)) * cell(0, i) * det_inner(minor(0, i))
      }).sum
    if square then det_inner(this) else throw NotSquareMatrixException()
  }
  /**
   * Multiply two matrices together. This is accomplished by taking the dot product of each row in the first with each
   * row in the second.
   *
   * {{{
   *  [a b][e f] = [ae+bg af+bh]
   *  [c d][g h]   [ce+dg cf+dh]
   * }}}
   *
   */
  def *(other: LegacyMatrix): LegacyMatrix = {
    if this == LegacyMatrix.identity(this.cols) then other
    else if other == LegacyMatrix.identity(this.cols) then this
    else if can_multiply(other) then LegacyMatrix((for (i <- 0 until rows) yield {
      (for (j <- 0 until other.cols) yield {
        row(i).zip(other.col(j)).map((a, b) => a * b).sum
      }).toList
    }).toList)
    else throw MultiplySizeMismatchException()
  }
  def *(other: Double): LegacyMatrix = map(n => n * other)
}

object LegacyMatrix {
  def apply(contents: List[List[Double]]): LegacyMatrix = { val mat = new LegacyMatrix(contents)
    if mat.valid_size then mat else throw InvalidDimensionException()
  }

  def identity(size: Int): LegacyMatrix = new LegacyMatrix ((for (i <- 0 until size) yield {(for (j <- 0 until size) yield {if i == j then 1d else 0d}).toList}).toList)
  def zero(size: Int): LegacyMatrix = new LegacyMatrix(List.fill(size)(List.fill(size)(0d)))
}