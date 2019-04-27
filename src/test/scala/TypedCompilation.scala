package org.hablapps.gist

import org.scalatest._
import cats.implicits._

class TypedCompilation extends FunSpec with Matchers{

  describe("Malformed expressions"){
    import UntypedCompilation.{Expr => UExpr}, UntypedCompilation.Expr.syntax._

    it("can be represented"){
      "_if(int(1), bool(true), bool(false))" should compile
    }

    import Tree.Deserializer.syntax._

    it("can be deserialized"){
      Node("if", List(Leaf("1"), Leaf("true"), Leaf("1"))).parse[UExpr] shouldBe
        Some(_if(int(1), bool(true), int(1)))
    }
  }

  sealed abstract class Expr[_]
  case class IntExpr(int: Int) extends Expr[Int]
  case class BoolExpr(bool: Boolean) extends Expr[Boolean]
  case class IfExpr[A](bool: Expr[Boolean], e1: Expr[A], e2: Expr[A]) extends Expr[A]

  object Expr{
    object syntax{
      def int(i: Int): Expr[Int] = IntExpr(i)
      def bool(b: Boolean): Expr[Boolean] = BoolExpr(b)
      def _if[A](b: Expr[Boolean], e1: Expr[A], e2: Expr[A]): Expr[A] = IfExpr(b, e1, e2)
    }
  }

  import Expr.syntax._

  describe("Malformed expressions (now)"){

    it("can't be represented"){
      // _if(int(1), bool(true), bool(false))
      "_if(int(1), bool(true), bool(false))" shouldNot compile
    }
  }

  import Tree.Serializer, Tree.Serializer.syntax._

  implicit def ExprSerializer[A]: Serializer[Expr[A]] = new Serializer[Expr[A]]{
    def show(e: Expr[A]): Tree = e match {
      case IntExpr(i) =>
        Leaf(i.toString)
      case BoolExpr(b) =>
        Leaf(b.toString)
      case IfExpr(b, e1, e2) =>
        Node("if", List(b.show, e1.show, e2.show))
    }
  }

  describe("Serialization"){
    import Expr.syntax._, Tree.Serializer.syntax._

    it("works"){
      _if(bool(true), int(1), int(2)).show shouldBe
        Node("if", List(Leaf("true"), Leaf("1"), Leaf("2")))
    }
  }

  import Tree.Deserializer, UntypedCompilation.ExprTreeMatchers._

  def WrongExprDeserializer: Deserializer[Expr[_]] = new Deserializer[Expr[_]]{
    def parse(a: Tree): Option[Expr[_]] = a match {
      case IntTree(i) =>
        Some(IntExpr(i))

      case BoolTree(b) =>
        Some(BoolExpr(b))

      case IfTree(tb, t1, t2) =>
        (parse(tb), parse(t1), parse(t2)).mapN{
          case (b, e1, e2) => ??? // IfExpr(eb, e1, e2)
        }
    }
  }

  sealed abstract class ExprType[T]
  implicit case object TInt extends ExprType[Int]
  implicit case object TBool extends ExprType[Boolean]

  object ExprType{
    def apply[A](implicit T: ExprType[A]) = T

    import cats.evidence.Is

    implicit object ExprTypeCast extends Cast[ExprType]{
      def as[T1, T2](t1: ExprType[T1], t2: ExprType[T2]): Option[T1 Is T2] =
        (t1, t2) match {
          case (TInt, TInt) => Some(Is.refl)
          case (TBool, TBool) => Some(Is.refl)
          case _ => None
        }
    }
  }

  import Cast.syntax._

  implicit def DynExprDeserializer = new Deserializer[Dynamic[Expr, ExprType]]{
    def parse(a: Tree): Option[Dynamic[Expr, ExprType]] = a match {
      case IntTree(i) =>
        Some(Dynamic(IntExpr(i)))

      case BoolTree(b) =>
        Some(Dynamic(BoolExpr(b)))

      case IfTree(tb, t1, t2) => for {
        (db, de1, de2) <- (parse(tb), parse(t1), parse(t2)).tupled
        b <- db.as[Boolean]
        e2 <- de2.as[de1.T](de1._type)
      } yield Dynamic(_if(b, de1.expr, e2))(de1._type)
    }
  }

  implicit def ExprDeserializer: Deserializer[Expr[_]] = new Deserializer[Expr[_]]{
    def parse(a: Tree): Option[Expr[_]] =
      DynExprDeserializer.parse(a).map(_.expr)
  }

  describe("Deserialization"){
    import Expr.syntax._, Serializer.syntax._, Deserializer.syntax._

    it("works for wellformed expressions"){
      _if(bool(true), int(1), int(2)).show.parse[Expr[_]] shouldBe
        Some(_if(bool(true), int(1), int(2)))
    }

    it("works for malformed expressions"){
      Node("if", List(Leaf("1"), Leaf("true"), Leaf("1"))).parse[Expr[_]] shouldBe
        None
    }
  }
}
