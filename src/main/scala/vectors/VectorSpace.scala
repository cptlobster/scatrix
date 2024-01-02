/*
    Trait for defining vector spaces.
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
package vectors

/**
 * This is a trait for an arbitrary type of vector space.
 *
 * = Implementation =
 *
 * To implement your own vector space using this trait, you must define the following functions:
 *
 * {{{
 *   class Vector extends VectorSpace {
 *     def ==(other: Vector): Boolean
 *     def +(other: Vector): Vector
 *     def *(other: Double): Vector
 *     def dot(other: Vector): Double
 *     val size: Int
 *   }
 * }}}
 * and all other functions are defined automatically based on those. For an example of a common vector space (R_3), you
 * can look at [[Vec3]]'s implementation.
 *
 * = Usage =
 * Vectors can be used with matrices. (coming soon)
 */
trait VectorSpace[T <: VectorSpace[T]]:
  /** Add two vectors together. */
  def ==(other: T): Boolean
  def +(other: T): T
  /** Multiply a vector by a scalar value. */
  def *(other: Double): T
  /** Subtract one vector from another. */
  def -(other: T): T = this + (other * -1f)
  /** Divide a vector by a scalar value. */
  def /(other: Double): T = *(1 / other)
  /** Get the dot product of two vectors. */
  def dot(other: T): Double
  /** Determine if two vectors are orthogonal. */
  def orthogonal(other: T): Boolean = dot(other) == 0
  /** Get the length of this vector. This is defined in terms of the dot product, like so:
   * {{{
   *   ||a|| = sqrt(<a,a>)
   * }}}
   * */
  def length: Double = math.sqrt(dot(this.*(1)))

  def distance(other: T): Double = (this - other).length

  def normalize: T = this / length

  val size: Int
