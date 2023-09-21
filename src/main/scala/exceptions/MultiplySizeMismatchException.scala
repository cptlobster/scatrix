/*
    Exception for mismatch in size in matrix multiplication
    Copyright (C) 2023  Dustin Thomas

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
package exceptions

final case class MultiplySizeMismatchException(private val message: String = s"The amount of columns in the left matrix should equal the amount of rows in the right matrix",
                                               private val cause: Throwable = None.orNull)
  extends Exception(message, cause)