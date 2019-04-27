package org.hablapps.gist

import org.scalatest._
import cats.implicits._

class UntypedCompilation extends FunSpec with Matchers{

  sealed abstract class Expr
  case class IntExpr(int: Int) extends Expr
  case class BoolExpr(bool: Boolean) extends Expr
  case class IfExpr(bool: Expr, e1: Expr, e2: Expr) extends Expr

  object Expr{
    object syntax{
      def int(i: Int): Expr = IntExpr(i)
      def bool(b: Boolean): Expr = BoolExpr(b)
      def _if(b: Expr, e1: Expr, e2: Expr): Expr = IfExpr(b, e1, e2)
    }
  }

  describe("Expressions"){
    import Expr.syntax._

    it("works"){
      _if(bool(true), int(1), int(2)) shouldBe
        IfExpr(BoolExpr(true), IntExpr(1), IntExpr(2))
    }
  }

  import Tree.Serializer

  implicit object ExprSerializer extends Serializer[Expr]{
    def show(e: Expr): Tree = e match {
      case IntExpr(i) =>
        Leaf(i.toString)
      case BoolExpr(b) =>
        Leaf(b.toString)
      case IfExpr(b, e1, e2) =>
        Node("if", List(show(b), show(e1), show(e2)))
    }
  }

  describe("Serialization"){
    import Expr.syntax._, Serializer.syntax._

    it("works"){
      _if(bool(true), int(1), int(2)).show shouldBe
        Node("if", List(Leaf("true"), Leaf("1"), Leaf("2")))
    }
  }

  object ExprTreeMatchers{
    import scala.util.Try

    object IntTree {
      def unapply(t: Tree): Option[Int] =
        t match {
          case Leaf(i) =>
            Try(Integer.parseInt(i)).toOption
          case _ =>
            None
        }
    }

    object BoolTree {
      def unapply(t: Tree): Option[Boolean] =
        t match {
          case Leaf("true") =>
            Some(true)
          case Leaf("false") =>
            Some(false)
          case _ =>
            None
        }
    }

    object IfTree {
      def unapply(t: Tree): Option[(Tree, Tree, Tree)] =
        t match {
          case Node("if", List(bt, e1t, e2t)) =>
            Some((bt, e1t, e2t))
          case _ =>
            None
        }
    }
  }

  import ExprTreeMatchers._, Tree.Deserializer

  implicit object ExprDeserializer extends Deserializer[Expr]{
    def parse(a: Tree): Option[Expr] = a match {
      case IntTree(i) => Some(IntExpr(i))
      case BoolTree(b) => Some(BoolExpr(b))
      case IfTree(tb, t1, t2) =>
        (parse(tb), parse(t1), parse(t2)).mapN{
          case (eb, e1, e2) => IfExpr(eb, e1, e2)
        }
    }
  }

  describe("Deserialization"){
    import Expr.syntax._, Serializer.syntax._, Deserializer.syntax._

    it("works"){
      _if(bool(true), int(1), int(2)).show.parse[Expr] shouldBe
        Some(_if(bool(true), int(1), int(2)))
    }
  }
}

object UntypedCompilation extends UntypedCompilation
