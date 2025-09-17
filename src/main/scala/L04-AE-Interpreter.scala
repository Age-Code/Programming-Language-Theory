import $ivy.`org.scala-lang.modules::scala-parser-combinators:2.4.0` // Used by Ammonite
import scala.util.parsing.combinator._

trait Expr
case class Num(num: Int) extends Expr
case class Add(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr) extends Expr

// [contract] interp : Expr => Int
def interp(expr: Expr): Int = expr match {
  case Num(n) => n
  case Add(l, r) => interp(l) + interp(r)
  case Sub(l, r) => interp(l) - interp(r)
}

// main
@main def run(): Unit = {
  val expr = Add(Num(3), Sub(Num(8), Num(2))) // "{+ 3 {- 8 2}}" << How to deal with this code???
  println(interp(expr))                                              // It leads to 9 as a result.
}