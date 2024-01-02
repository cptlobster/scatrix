/*
    Example Vector3 class
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

import vectors.VectorSpace

class Vec3(val x: Double, val y: Double, val z: Double) extends VectorSpace[Vec3]:
  // This is an example of defining your own vector space. In this case, we're defining R_3. In this first part, we just
  // declare the minimal elements of the class (the rest is determined by our [[VectorSpace]] trait!)
  def ==(other: Vec3): Boolean = x == other.x && y == other.y && z == other.z
  def +(other: Vec3): Vec3 = Vec3(x + other.x, y + other.y, z + other.z)
  def *(other: Double): Vec3 = Vec3(x * other, y * other, z * other)
  def dot(other: Vec3): Double = (x * other.x) + (y * other.y) + (z * other.z)
  val size: Int = 3
  // Since this is a 3-dimensional vector, we can also declare the cross product!
  /** Take the cross product of two vectors (return the orthogonal complement). */
  def cross(other: Vec3): Vec3 = ???

object Vec3:
  def apply(x: Double, y: Double, z: Double): Vec3 = new Vec3(x, y, z)
  def zero: Vec3 = new Vec3(0, 0, 0)