package dev.cptlobster.scatrix

import org.scalatest.funsuite.AnyFunSuite

class MatrixSuite extends AnyFunSuite {
  val example_1: Matrix = Matrix(List(List(1,2,3),List(4,5,6),List(7,8,9)))
  val example_2: Matrix = Matrix(List(List(9,2,0),List(6,2,4),List(3,1,1)))
  val example_3: Matrix = Matrix(List(List(1,2,3,4),List(5,6,7,8),List(9,10,11,12)))
  val example_id3: Matrix = Matrix.identity(3)
  val example_id2: Matrix = Matrix.identity(2)

  test("Matrix selects correct cells") {
    assert(example_1.cell(2, 2) == 9)
    assert(example_1.cell(0, 1) == 2)
    assert(example_2.cell(0, 0) == 9)
  }

  test("Square matrices should have the same rows/columns") {
    assert(example_1.square)
    assert(!example_3.square)
  }

  test("Matrix minor removes correct rows/columns") {
    assert(example_1.minor(0, 0) == Matrix(List(List(5,6),List(8,9))))
  }

  test("Determinant function properly calculates the determinant of the matrix") {
    assert(example_1.det == 0)
    assert(example_2.det == -6)
    assert(example_id2.det == 1)
  }

  test("Matrix multiply properly multiplies matrices") {
    assert((example_1 * example_2).cell(0, 0) == 30)
    assert((example_1 * example_id3) == example_1)
  }
}
