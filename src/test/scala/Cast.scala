package org.hablapps.gist

import cats.evidence.Is

trait Cast[Type[_]]{
  def as[T1, T2](t1: Type[T1], t2: Type[T2]): Option[T1 Is T2]

  def as[Repr[_], T2](d: Dynamic[Repr, Type], t2: Type[T2]): Option[Repr[T2]] =
    as(d._type, t2).map(_.substitute[Repr](d.expr))
}

object Cast{
  object syntax{
    implicit class CastOps[Repr[_], Type[_]](d: Dynamic[Repr, Type])(implicit C: Cast[Type]){
      def as[T2](implicit t2: Type[T2]): Option[Repr[T2]] =
        C.as(d, t2)
    }
  }
}
