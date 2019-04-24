package org.hablapps.gist

import org.scalatest._
import cats.implicits._

class UntypeCompilation extends FunSpec with Matchers{

  sealed abstract class Expr
  case class IntExpr(int: Int) extends Expr
  case class BoolExpr(bool: Boolean) extends Expr
  case class IfExpr(bool: Expr, e1: Expr, e2: Expr) extends Expr

  sealed abstract class Tree
  case class Leaf(s: String) extends Tree
  case class Node(root: String, childs: List[Tree]) extends Tree

  trait Deserializer[A]{
    def parse(a: Tree): Option[A]
  }

  object Deserializer{
    implicit object ExprDeserializer extends Deserializer[Expr]{

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

      def parse(a: Tree): Option[Expr] = a match {
        case IntTree(i) => Some(IntExpr(i))
        case BoolTree(b) => Some(BoolExpr(b))
        case IfTree(tb, t1, t2) =>
          (parse(tb), parse(t1), parse(t2)).mapN{
            case (eb, e1, e2) => IfExpr(eb, e1, e2)
          }
      }
    }
  }
}
