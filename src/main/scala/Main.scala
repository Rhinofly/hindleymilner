import scalaz._
import Scalaz._

import Inference._
import Examples._
import Types._
import AST._

object Main {
  def main(args:Array[String]):Unit = {
    exec(test1)
    exec(test2)
  }
  
  def exec(e:Expr):Unit = {  
    print(e.show)
    print(" :: ")
    
    infer(defaultEnv)(e).run.eval(0) match {
      case -\/(message)    => println(message)
      case \/-((ty,subst)) => println(ty.show)
    }
  }
}