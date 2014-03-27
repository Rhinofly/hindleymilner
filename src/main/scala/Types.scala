import scalaz._
import Scalaz._

import AST._

object Types {
  type TyLabel = scala.Int

  sealed trait Ty
  case object TyBool extends Ty
  case object TyInt extends Ty
  case class TyVar(v: TyLabel) extends Ty
  case class TyFun(f: Ty, x: Ty) extends Ty
  
  object Ty {
    implicit def ShowOnTy = new Show[Ty] {
      override def shows(ty:Ty):String = ty match {
        case TyBool     => "bool"
        case TyInt      => "int"
        case TyVar(n)   => (n+97).toChar.toString
        case TyFun(l,r) => shows(l) + " -> " + shows(r)
      }
    }
  }
  
  implicit class TypesOps[T](t:T)(implicit e: Types[T]) {
    def applySubst[A](subst: Subst): T = e.apply(subst)(t)
    def ftv[A](a: A): Set[TyLabel] = e.ftv(t)
  }

  type Subst = Map[TyLabel, Ty]
  
  object Subst {
    def empty = Map.empty[TyLabel, Ty]
  }

  implicit class SubstOps(s1: Subst)(implicit e: Types[Ty]) {
    val applySubsts = e.apply(s1)(_: Ty)
    def @@(s2: Subst): Subst = (s2 mapValues applySubsts) ++ s1
  }
  
  type Env = Map[VarName, Ty]
  
  val defaultEnv: Env = Map.empty
  
  trait Types[A] {
    def apply(subst: Subst)(a: A): A
    def ftv(a: A): Set[TyLabel]
  }

  object Types {
    implicit val TypesOnTy: Types[Ty] = new Types[Ty] {
      def apply(subst: Subst)(ty: Ty): Ty = ty match {
        case x @ (TyBool | TyInt) => x
        case TyVar(v)    => (subst get v).map(x => x).getOrElse(TyVar(v))
        case TyFun(f, x) => TyFun(apply(subst)(f), apply(subst)(x))
      }
      def ftv(ty: Ty) = ty match {
        case TyVar(v)    => Set(v)
        case otherwise   => Set.empty[TyLabel]
      }
    }

    implicit def TypesOnEnv: Types[Env] = new Types[Env] {
      def apply(subst: Subst)(env: Env): Env = {
        val e = implicitly[Types[Ty]]
        env mapValues (e.apply(subst))
      }
      def ftv(env: Env): Set[TyLabel] = {
        val e = implicitly[Types[Ty]]
        env.values.map(e.ftv).fold(Set.empty)((x, y) => x ++ y)
      }
    }
  }
  
}