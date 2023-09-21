package dev.cptlobster.scatrix

import org.scalatest.funsuite.AnyFunSuite

class ElementaryOpsSuite extends AnyFunSuite {
  test("Row swap works") {
    assert(ElementaryOps(3).swap(0,1).mat.cell(0, 1) == 1)
    assert(ElementaryOps(3).swap(2,0).mat.cell(0, 2) == 1)
    assert(ElementaryOps(3).swap(0,2).mat.cell(0, 2) == 1)
  }

  test("Scalar multiply works") {
    assert(ElementaryOps(3).mult(0, 5).mat.cell(0, 0) == 5)
    assert(ElementaryOps(3).mult(2, 3).mat.cell(2, 2) == 3)
  }

  test("Addition works") {
    assert(ElementaryOps(3).add(2, 0).mat.cell(0, 2) == 1)
    assert(ElementaryOps(3).add(1, 0, 3).mat.cell(0, 1) == 3)
    assert(ElementaryOps(3).add(2, 1, 4).mat.cell(1, 2) == 4)
  }
}
