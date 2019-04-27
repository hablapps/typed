package org.hablapps.gist

abstract class Dynamic[Repr[_], Type[_]]{
  type T
  val expr: Repr[T]
  val _type: Type[T]
}

object Dynamic{
  def apply[_T, Repr[_], Type[_]](_e: Repr[_T])(implicit TR: Type[_T]): Dynamic[Repr, Type] = new Dynamic[Repr, Type]{
    type T = _T
    val _type = TR
    val expr = _e
  }
}
