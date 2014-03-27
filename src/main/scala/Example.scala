import AST._

object Examples {
  def test1:Expr = Let("x", LBool(true), Var("x"))

  def test2:Expr = Lambda(
    "m",
    Let(
      "y",
      Var("m"),
      Let(
        "x",
        App(
          Var("y"),
          LBool(true)),
        Var("x"))))
}