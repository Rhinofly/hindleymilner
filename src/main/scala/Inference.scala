import scalaz._
import Scalaz._
import scala.language.implicitConversions

object Inference {
  import AST._
  import Types._

  type Inner[+A] = State[Int, A]
  type TI[A] = EitherT[Inner, String, A]

  object TI {
    def apply[A](a: A): TI[A] = a.point[TI]
  }

  implicit def liftToTI[A](x: Inner[A]): TI[A] = {
    EitherT(x.map(_.right): Inner[Nothing \/ A])
  }

  def throwError[A]: String => TI[A] = { message => 
    EitherT.eitherT[Inner, String, A](
      StateT.stateT[Id, Int, String \/ A](message.left)
    )
  }

  def infer: Env => Expr => TI[(Ty, Subst)] = { env => e => 
    e match {
      case LBool(b) => TI(TyBool -> Subst.empty)
      case LInt(i) => TI(TyInt -> Subst.empty)
      case Var(v) => {
        (env get v) match {
          case None => throwError("Unknown variable")
          case Some(ty) => TI(ty -> Subst.empty)
        }
      }
  
      case Let(v, body, e) => for {
        (t1, s1) <- infer(env)(body)
        (t2, s2) <- infer((env + (v -> t1)).applySubst(s1))(e)
      } yield (t2, s1 @@ s2)
  
      case App(f, x) => for {
        tv <- fresh
        (t1, s1) <- infer(env)(f)
        (t2, s2) <- infer(env applySubst s1)(x)
        s <- unify(t1)(TyFun(t2, tv))
      } yield (tv applySubst s, s @@ s2 @@ s1)
  
      case Lambda(v, e) => for {
        tv <- fresh
        (t1, s1) <- infer(env + (v -> tv))(e)
      } yield (TyFun(tv applySubst s1, t1), s1)
    }
  }

  def fresh: TI[Ty] = for {
    counter <- get[Int]
    _ <- put[Int](counter + 1)
  } yield TyVar(counter)

  def unify: Ty => Ty => TI[Subst] = { l => r =>
    (l, r) match {
      case (TyBool, TyBool) => TI(Subst.empty)
      case (TyInt, TyInt) => TI(Subst.empty)
      case (TyVar(v), ty) => bind(v)(ty)
      case (ty, TyVar(v)) => bind(v)(ty)
      case (TyFun(l1, r1), TyFun(l2, r2)) => for {
        s1 <- unify(l1)(l2)
        s2 <- unify(r1 applySubst s1)(r2 applySubst s1)
      } yield s2 @@ s1
      case otherwise => throwError("Types don't unify")
    }
  }

  def bind: TyLabel => Ty => TI[Subst] = {v => ty =>
    if (ty.equals(TyVar(v))) {
      TI(Subst.empty)
    } else if (ty.ftv() contains v) {
      throwError("occurs check failed")
    } else {
      TI(Map(v -> ty))
    }
  }
}