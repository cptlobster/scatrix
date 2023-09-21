package dev.cptlobster.scatrix
package exceptions

final case class NotSquareMatrixException(private val message: String = "Operation requires a square matrix.",
                                          private val cause: Throwable = None.orNull)
  extends Exception(message, cause) 
