package dev.cptlobster.scatrix

import org.scalatest.funsuite.AnyFunSuite

class VectorSuite extends AnyFunSuite {
  val u: Vec3 = Vec3(2, 3, 1)
  val v: Vec3 = Vec3(0, 1, 4)
  val w: Vec3 = Vec3(2, 0, 0)
  val zero: Vec3 = Vec3(0, 0, 0)

  test("Vector addition") {
    // addition
    assert(u + v == Vec3(2, 4, 5))
    // subtraction
    assert(u - v == Vec3(2, 2, -3))
    assert(u - w == Vec3(0, 3, 1))
    // check that subtracting a vector from itself returns zero
    assert(u - u == zero)
    // adding the zero vector returns the original vector
    assert(u + zero == u)
  }

  test("Scalar Multiplication") {
    // multiplying by a scalar
    assert(u * 2 == Vec3(4, 6, 2))
    // multiplying by zero returns the zero vector
    assert(u * 0 == zero)
    // multiplying by a negative number
    assert(v * -3 == Vec3(0, -3, -12))
    // dividing (multiplying by a fraction)
    assert(v / 2 == Vec3(0, 0.5, 2))
  }

  test("Dot Product") {
    // taking the dot product of two vectors
    assert((u dot v) == 7)
    // dot product of two orthogonal vectors returns zero
    assert((v dot w) == 0)
  }

  test("Orthogonal") {
    // same as v dot w (should be true)
    assert(v.orthogonal(w))
  }

  test("Length") {
    // get the length of a vector with a single axis
    assert(w.length == 2)
    // get the length of a vector with multiple axes
    assert(u.length == math.sqrt(14))
    // length of zero is zero
    assert(zero.length == 0)
  }
}