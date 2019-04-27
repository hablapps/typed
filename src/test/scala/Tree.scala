package org.hablapps.gist

import org.scalatest._
import cats.implicits._

sealed abstract class Tree
case class Leaf(s: String) extends Tree
case class Node(root: String, childs: List[Tree]) extends Tree

object Tree{
  trait Serializer[A]{
    def show(a: A): Tree
  }

  object Serializer{
    object syntax{
      implicit class SerializerOps[A](a: A)(implicit S: Serializer[A]){
        def show: Tree = S.show(a)
      }
    }
  }

  trait Deserializer[A]{
    def parse(a: Tree): Option[A]
  }

  object Deserializer{

    object syntax{
      implicit class DeserializerOps(t: Tree){
        def parse[A](implicit D: Deserializer[A]): Option[A] = D.parse(t)
      }
    }
  }
}
