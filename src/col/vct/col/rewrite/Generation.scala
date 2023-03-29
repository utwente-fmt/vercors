package vct.col.rewrite

sealed trait Generation
sealed trait InitialGeneration extends Generation
sealed trait Rewritten[T <: Generation] extends Generation
