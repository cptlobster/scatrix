package dev.cptlobster.scatrix
package exceptions

final case class SizeException(private val message: String = "The sizes of matrices you are trying to operate on do not match.",
                               private val cause: Throwable = None.orNull)
  extends Exception(message, cause) 
