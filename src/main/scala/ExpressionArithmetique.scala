/**
 * Arithmetic expression representation.
 *
 * In this expressions you can only have a unique variable.
 * eg. Mult(Var, Add(Const(2), Var)) represents X * (2 + X)
 */
sealed trait Expression
object Expression {
  // represent a variable
  case object Var extends Expression
  // represent a constant
  case class Const(value: Double) extends Expression

  case class Add(left: Expression, right: Expression) extends Expression

  case class Mult(left: Expression, right: Expression) extends Expression

  /**
   * Evaluate an expression for a given value for the variable.
   *
   * eg. eval(Mult(Var, Add(Const(2), Var)))(3) => 15
   */
  def eval(expression: Expression)(variable: Double): Double = expression match {
    case Const(v) => v
    case Add(left,right) => eval(left)(variable) + eval(right)(variable)
    case Mult(left,right) => eval(left)(variable) * eval(right)(variable)

  }

  /**
   * Convert an expression to its postfix representation.
   *
   * eg. Mult(Var, Add(Const(2), Var)) => X 2 X + *
   */
  def toPostfix(expression: Expression): String = ???

  /**
   * Convert an expression to its prefix representation.
   *
   * eg. Mult(Var, Add(Const(2), Var)) => (* X (+ 2 X))
   */
  def toPrefix(expression: Expression): String = ???

  /**
   * Convert an expression to its infix representation.
   *
   * eg. Mult(Var, Add(Const(2), Var)) => X * (2 + X)
   */
  def toInfix(expression: Expression): String = ???

  /**
   * Deriv an expression according to the variable.
   *
   * eg. deriv(Mult(Var, Add(Const(2), Var))) =>
   *  Add(Mult(Const(1.0), Add(Const(2.0), Var)),
   *      Mult(Var, Add(Const(0.0), Const(1.0))))
   */
  def deriv(expression: Expression): Expression = ???

 /**
  * Bonus :
  * Get the polynomial coeficients from the expression
  *  eg.
  *  val exp1 = Plus(Var,Const(3))
  *  polyCoef(Mult(exp1,exp1)) => Vector(9,6,1)
  */
  def polyCoef(expression: Expression): Vector[Double] = ???
}