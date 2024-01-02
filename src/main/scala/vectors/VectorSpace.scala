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
 * = Implementation =
 * To implement your own vector space using this trait, you must define the following functions:
 * {{{
 *   class VecExample extends VectorSpace[VecExample] {
 *     def ==(other: VecExample): Boolean
 *     def +(other: VecExample): VecExample
 *     def *(other: Double): VecExample
 *     def dot(other: VecExample): Double
 *     val size: Int
 *   }
 * }}}
 * and all other functions are defined automatically based on those. For an example of a common vector space (R_3), you
 * can look at [[Vec3]]'s implementation.
 *
 * It is also recommended that you define `toString`, but is not required by the trait.
 * = Usage =
 * Vectors can be used with matrices. (coming soon)
 * = Examples =
 * For a 3-dimensional vector space V with standard addition and multiplication:
 * {{{
 *   val u = V(1, 2, 3)
 *   val v = V(0, 3, 1)
 *   val k = 2
 *
 *   u + v = V(1, 5, 4)
 *   ku = V(2, 4, 6)
 *   ku + v = V(2, 7, 7)
 *   k dot u = (1 * 0) + (2 * 3) + (3 * 1) = 0 + 6 + 3 = 9
 * }}}
 *
 * @tparam T Recursive type parameter, necessary for function assignment. When creating a class, use the class as your
 *           type parameter, and all the functions will theoretically work.
 */
trait VectorSpace[T <: VectorSpace[T]]:
  /** Determine if two vectors are equal.
   * @param other Another vector of the same type.
   * @return True if equal, false otherwise.
   **/
  def ==(other: T): Boolean
  /** Add two vectors together.
   * @param other Another vector of the same type.
   * @return The sum of the two vectors. */
  def +(other: T): T
  /** Multiply a vector by a scalar value.
   * @param other A scalar value.
   * @return The product of each component of the vector and the scalar. */
  def *(other: Double): T
  /** Subtract one vector from another.
   * @param other Another vector of the same type.
   * @return The difference of the two vectors. */
  def -(other: T): T = this + (other * -1f)
  /** Divide a vector by a scalar value.
   * @param other A scalar value.
   * @return The quotient of each component of the vector (divisor) and the scalar (dividend). */
  def /(other: Double): T = *(1 / other)
  /** Get the dot product of two vectors.
   * @param other Another vector of the same type.
   * @return The dot product (a scalar value). */
  def dot(other: T): Double
  /** Determine if two vectors are orthogonal.
   * @param other Another vector of the same type.
   * @return True if the vectors are orthogonal (u dot v == 0), false otherwise. */
  def orthogonal(other: T): Boolean = dot(other) == 0
  /** Get the length of this vector. This is defined in terms of the dot product, like so:
   * {{{
   *   ||a|| = sqrt(<a,a>)
   * }}}
   * @return The length, as a scalar value. */
  def length: Double = math.sqrt(dot(this.*(1)))
  /** Get the distance between two vectors. This is defined in terms of [[VectorSpace.length]], like so:
   * {{{
   *   a.distance(b) = ||a - b|| = sqrt(<a - b, a - b>)
   * }}}
   * @param other Another vector of the same type.
   * @return The distance, as a scalar value. */
  def distance(other: T): Double = (this - other).length
  /** Normalize the vector to a length of 1 by dividing by the current length. ''Note that due to floating point
   * precision issues, length may not exactly be 1, but should be close enough.''
   * @return A vector of the original type, normalized. */
  def normalize: T = this / length

  val size: Int
