/*
    Standard n-dimensional vector class. This is the default vector used with matrices.
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

import exceptions.InvalidDimensionException

class Vector(val items: List[Double]) extends VectorSpace[Vector]:
  private def force_size(other: Vector): Boolean = if size == other.size then true else throw InvalidDimensionException()
  def == (other: Vector): Boolean = items.zip(other.items).forall((a, b) => a == b)
  def + (other: Vector): Vector =
    force_size(other)
    Vector(items.zip(other.items).map((a, b) => a + b))
  def * (other: Double): Vector =
    Vector(items.map(x => x * other))
    
  val size: Int = items.size
  
  def dot(other: Vector): Double = 
    force_size(other)
    items.zip(other.items).map((a, b) => a * b).sum

object Vector:
  def apply(items: List[Double]): Vector = new Vector(items)
  def zero(size: Int): Vector = new Vector((for (i <- 0 to size) yield 0d).toList)