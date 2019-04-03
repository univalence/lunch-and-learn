import scala.annotation.tailrec

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
    case Add(left, right) => eval(left)(variable) + eval(right)(variable)
    case Mult(left, right) => eval(left)(variable) * eval(right)(variable)
    case Var => variable
  }
  
  

  def reduceStack[T](stack:List[T])(rules:PartialFunction[List[T],List[T]]):List[T] = {
    @tailrec
    def go(stack:List[T])  :List[T] = go(stack) // ???

    go(stack)
  }

  def evalTailRec(expression: Expression)(variable: Double): Double = {
    sealed trait Stack
    case class Ct(value: Double) extends Stack
    case object AddOp extends Stack
    case object MultOp extends Stack
    case class ExpOp(expression: Expression) extends Stack

    @tailrec
    def go(stack: List[Stack]): Double = {
      stack match {
        case Ct(v) :: Nil => v
        case Ct(a) :: Ct(b) :: AddOp :: rest => go(Ct(a + b) :: rest)
        case Ct(a) :: Ct(b) :: MultOp :: rest => go(Ct(a * b) :: rest)
        case ExpOp(Const(v)) :: rest => go(Ct(v) :: rest)
        case ExpOp(Add(left, right)) :: rest => go(ExpOp(left) :: ExpOp(right) :: AddOp :: rest)
        case ExpOp(Mult(left, right)) :: rest => go(ExpOp(left) :: ExpOp(right) :: MultOp :: rest)
        case ExpOp(Var) :: rest => go(Ct(variable) :: rest)
        case x :: y :: rest => go(y :: x :: rest)
      }
    }

    go(ExpOp(expression) :: Nil)
  }

  /**
    * Convert an expression to its postfix representation.
    *
    * eg. Mult(Var, Add(Const(2), Var)) => X 2 X + *
    */
  def toPostfix(expression: Expression): String = expression match {
    case Const(v) => v.toString
    case Add(left, right) => toPostfix(left) + " " + toPostfix(right) + " +"
    case Mult(left, right) => toPostfix(left) + " " + toPostfix(right) + " *"
    case Var => "X"
  }

  /**
    * Convert an expression to its prefix representation.
    *
    * eg. Mult(Var, Add(Const(2), Var)) => (* X (+ 2 X))
    */
  def toPrefix(expression: Expression): String = {
    expression match {
      case Const(v) => v.toString
      case Add(left, right) => "(+ " + toPrefix(left) + " " +  toPrefix(right) + ")"
      case Mult(left, right) => "(* " + toPrefix(left) + " " + toPrefix(right) + ")"
      case Var => "X"
    }
  }

  /**
    * Convert an expression to its infix representation.
    *
    * eg. Mult(Var, Add(Const(2), Var)) => X * (2 + X)
    */
  def toInfix(expression: Expression): String = {
    expression match {
      case Const(v) => v.toString
      case Add(left, right) => "(" + toInfix(left) + " + " + toInfix(right) + ")"
      case Mult(left, right) => "(" + toInfix(left) + " * " + toInfix(right) + ")"
      case Var => "X"
    }
  }

  def derivTailRec(expression: Expression): Expression = {
    // |> |> Tailrec Harrison <| <|
    // DerviOP , KeepOp, AddOp, MultOp 

    sealed trait Stack
    sealed trait Op extends Stack
    case object AddOp extends Op
    case object MultOp extends Op
    case object DerivOp extends Op
    case class ExprOp(expr:Expression) extends Stack


    def go(stack: List[Stack]): Expression = {

      val idx = stack.indexWhere(_.isInstanceOf[Op])

      if(idx == -1 ) {
        stack.head.asInstanceOf[ExprOp].expr
      } else {

        val prefix = stack.take(idx).reverse
        val suffix = stack.drop(idx + 1)
        val op: Op = stack(idx).asInstanceOf[Op]

        op match {
          case DerivOp =>
            val derived: List[Stack] = prefix.head match {
              case ExprOp(Var) => ExprOp(Const(1)) :: Nil
              case ExprOp(Const(_)) => ExprOp(Const(0)) :: Nil
              case ExprOp(Add(left, right)) => ExprOp(left) :: DerivOp :: ExprOp(right) :: DerivOp :: AddOp :: Nil
              case ExprOp(Mult(left, right)) => ExprOp(left) :: DerivOp :: ExprOp(right) :: MultOp :: ExprOp(left) :: ExprOp(right) :: DerivOp :: MultOp :: AddOp :: Nil
            }
            go(prefix.tail.reverse ::: derived ::: suffix)

          case AddOp | MultOp =>
            val ExprOp(x) :: ExprOp(y) :: rest = prefix
            val expr = op match {
              case AddOp => Add(x, y)
              case MultOp => Mult(x, y)
            }
            go(rest.reverse ::: List(ExprOp(expr)) ::: suffix)
        }
      }
    }
    go(ExprOp(expression) :: DerivOp :: Nil)

  }

  /**
    * Deriv an expression according to the variable.
    *
    * eg. deriv(Mult(Var, Add(Const(2), Var))) =>
    * Add(Mult(Const(1.0), Add(Const(2.0), Var)),
    * Mult(Var, Add(Const(0.0), Const(1.0))))
    *
    * (u * v)' = u' * v + u * v'
    */
  def deriv(expression: Expression): Expression = {
    expression match {
      case Const(_) => Const(0.0)
      case Var => Const(1.0)
      case Add(left,right) => Add(deriv(left),deriv(right))
      case Mult(left,right) => Add(Mult(deriv(left),right),Mult(left,deriv(right)))
    }
  }

  /**
    * Bonus :
    * Get the polynomial coeficients from the expression
    * eg.
    * val exp1 = Plus(Var,Const(3))
    * polyCoef(Mult(exp1,exp1)) => Vector(9,6,1)
    */

  //TODO : @Bernarith
  //addVector(Vector(1,2), Vector(0,3,0,4)) == Vector(1,5,0,4)
  def addVector(left:Vector[Double], right:Vector[Double]):Vector[Double] = {

    if(left.size < right.size) {
   ???
    }
    ???
  }

  def polyCoef(expression: Expression): Vector[Double] = {
    expression match {
      case Const(v) => Vector(v)
      case Var => Vector(0,1)
      case Add(left,right) => addVector(polyCoef(left),polyCoef(right))
    }
  }
}


object Main {
  def main(args: Array[String]): Unit = {

    import Expression._

    // eval function
    assert(eval(Var)(1) == 1)
    assert(eval(Const(2))(1) == 2)
    assert(eval(Mult(Var, Add(Const(2), Var)))(3) == 15)

    // eval tailrec function
    assert(evalTailRec(Var)(1) == 1)
    assert(evalTailRec(Const(2))(1) == 2)
    assert(evalTailRec(Mult(Var, Add(Const(2), Var)))(3) == 15)

    // toPostFix function
    assert(toPostfix(Var) == "X")
    assert(toPostfix(Const(2)) == "2.0")
    assert(toPostfix(Mult(Var, Add(Const(2), Var))) == "X 2.0 X + *")

    // toPrefix function
    assert(toPrefix(Var) == "X")
    assert(toPrefix(Const(2)) == "2.0")
    assert(toPrefix(Mult(Var, Add(Const(2), Var))) == "(* X (+ 2.0 X))")

    // toInfix function
    assert(toInfix(Var) == "X")
    assert(toInfix(Const(2)) == "2.0")
    assert(toInfix(Mult(Var, Add(Const(2), Var))) == "(X * (2.0 + X))")

    // deriv function
    assert(deriv(Var) == Const(1))
    assert(deriv(Const(2)) == Const(0))
    assert(deriv(Mult(Var, Add(Const(2), Var))) == Add(Mult(Const(1.0), Add(Const(2.0), Var)), Mult(Var, Add(Const(0.0), Const(1.0)))))


    // deriv tail rec function
    //assert(derivTailRec(Var) == Const(1))
    //assert(derivTailRec(Const(2)) == Const(0))
    //assert(derivTailRec(Add(Const(2), Var)) == Add(Const(1.0), Const(0.0)))
    println(derivTailRec(Mult(Var, Add(Const(2), Var))))
    assert(derivTailRec(Mult(Var, Add(Const(2), Var))) == Add(Mult(Add(Const(1.0),Const(0.0)),Var),Mult(Add(Const(2.0),Var),Const(1.0))))
    //println(derivTailRec(Mult(Add(Const(2), Var), Var)))
  }
}