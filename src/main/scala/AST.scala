import scalaz.Show

object AST {
  type VarName = String
  
  sealed trait Expr
  case class LBool(b:scala.Boolean) extends Expr
  case class LInt(i:scala.Int) extends Expr
  case class Var(v:VarName) extends Expr
  case class Let(v:VarName, body:Expr, e:Expr) extends Expr
  case class App(f:Expr, x:Expr) extends Expr
  case class Lambda(v:VarName, e:Expr) extends Expr 
    
  implicit def ShowOnExpr = new Show[Expr] {
    override def shows(expr:Expr):String = expr match {
      case LBool(b)      => b.toString
      case LInt(i)       => i.toString
      case Var(v)        => v
      case Let(v,body,e) => "let " + v + " = " + show(body) + " in " + show(e)
      case App(f,x)      => show(f) + " (" + show(x) + ")"
      case Lambda(v,e)   => "\\" + v + " -> " + show(e)
    }
  }
}