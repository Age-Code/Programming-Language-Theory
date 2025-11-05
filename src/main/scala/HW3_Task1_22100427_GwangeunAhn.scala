// (2 points) Implement FAE in Scala. Submit your scala file in LMS
// Solved by myself: Y
// (I referred to the course materials)
// 06-07-08-Substitution, 09-10-Functions, 11-Deferring Substitution, 12-13-14-First-Class Functions
// Time taken: about 78 mins

import $ivy.`org.scala-lang.modules::scala-parser-combinators:2.4.0` // Used by Ammonite
import scala.util.parsing.combinator._

trait Expr
case class Num(n: Int) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Sub(lhs: Expr, rhs: Expr) extends Expr
case class With(name: Id, nameExp: Expr, body: Expr) extends Expr
case class Id(name: String) extends Expr
case class Fun(param: Id, body: Expr) extends Expr
case class App(ftn: Expr, arg: Expr) extends Expr

class SimpleException(message: String) extends RuntimeException(message) {
  override def fillInStackTrace(): Throwable = this
  override def printStackTrace(): Unit = println(message)
}

// [contract] parse: String -> Expr
// [purpose] To convert concrete syntax string into an Expr abstract syntax tree
// [tests] parse("{with {x 3} {+ x x}}") => App(Fun(Id(x), Add(Id(x), Id(x))), Num(3))
//         parse("{with {y 10} {fun {x} {+ y x}}}") => App(Fun(Id(y),Fun(Id(x),Add(Id(y),Id(x)))),Num(10))
def parse(input: String): Expr = {
    object Parser extends RegexParsers {
        def int: Parser[Int] = """\d+""".r ^^ { _.toInt }
        def symbol: Parser[Id] = """[a-zA-Z_]\w*""".r ^^ { s => Id(s) }
        def wrap[T](parser: Parser[T]): Parser[T] = "{" ~> parser <~ "}"

        lazy val expr: Parser[Expr] =
            int ^^ { case n => Num(n) } |
            symbol |
            wrap("+" ~> expr ~ expr) ^^ { case l ~ r => Add(l, r) } |
            wrap("-" ~> expr ~ expr) ^^ { case l ~ r => Sub(l, r) } |
            wrap("with" ~> wrap(symbol ~ expr) ~ expr) ^^ {case (Id(name) ~ value) ~ body => App(Fun(Id(name), body), value)} |
            wrap("fun" ~> wrap(symbol) ~ expr) ^^ {case Id(param) ~ body => Fun(Id(param), body)} |
            wrap(expr ~ expr) ^^ { case ftn ~ arg => App(ftn, arg) }
        def parseAllExpr(str: String): Expr =
            parseAll(expr, str).getOrElse(throw new SimpleException(s"bad syntax: $str"))
    }
    Parser.parseAllExpr(input)
}

trait ExprValue
case class NumV(n: Int) extends ExprValue
case class ClosureV(param: Id, body: Expr, ds: DefrdSub) extends ExprValue

trait DefrdSub
case object MtSub extends DefrdSub
case class ASub(name: Id, value: ExprValue, saved: DefrdSub) extends DefrdSub

// [contract] numOperator: ((Int, Int) => Int): ((ExprValue, ExprValue) => ExprValue)
// [purpose] To calulate numerical operations
// [tests] numOperator(_+_) (NumV(1), NumV(2)) => NumV(3)
//         numOperator(_-_) (NumV(2), NumV(1)) => NumV(1)
def numOperator(op: (Int, Int) => Int): (ExprValue, ExprValue) => ExprValue = {
    case (NumV(x), NumV(y)) => NumV(op(x, y))
    case _ => throw new SimpleException("Expected numerical expressions")
}
val numAdd: (ExprValue, ExprValue) => ExprValue = numOperator(_ + _)
val numSub: (ExprValue, ExprValue) => ExprValue = numOperator(_ - _)

// [contract] lookup: (Id, DefrdSub) -> ExprValue
// [purpose] To find the value of an identifier in the deferred substitution environment(ds)
// [tests] lookup(Id("x"), ASub(Id("x"), NumV(10), MtSub)) => NumV(10)
//         lookup(Id("y"), ASub(Id("x"), NumV(5), ASub(Id("y"), NumV(10), MtSub))) => NumV(10)
def lookup(name: Id, ds: DefrdSub): ExprValue = ds match {
    case MtSub => throw new SimpleException(s"Free Identifier: $name")
    case ASub(i, v, saved) => if (i == name) v else lookup(name, saved)
}

// [contract] interp: (Expr, DefrdSub) -> ExprValue
// [purpose] To evaluate an FAE expression.
// [tests] interp(parse("{+ 1 2}"), MtSub) => NumV(3)
// [tests] interp(parse("{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}}"), MtSub) => NumV(7)
def interp(expr: Expr, ds: DefrdSub): ExprValue = expr match {
    case Num(n) => NumV(n)
    case Add(l, r) => numAdd(interp(l, ds), interp(r, ds))
    case Sub(l, r) => numSub(interp(l, ds), interp(r, ds))
    case Id(s) => lookup(Id(s), ds)
    case Fun(p, b) => ClosureV(p, b, ds)
    case App(ftn, arg) =>
        val f_val = interp(ftn, ds)
        val a_val = interp(arg, ds)
        f_val match {
            case ClosureV(param, body, closure_ds) => interp(body, ASub(param, a_val, closure_ds))
            case _ => throw new SimpleException("Expected a function")
        }
}

// main
@main def run(): Unit = {

    // parse
    assert(parse("{with {x 3} {+ x x}}") == App(Fun(Id(x), Add(Id(x), Id(x))), Num(3)), "Test failed! parse(\"{with {x 3} {+ x x}}\") == App(Fun(Id(x), Add(Id(x), Id(x))), Num(3))")
    assert(parse("{with {y 10} {fun {x} {+ y x}}}") == App(Fun(Id(y),Fun(Id(x),Add(Id(y),Id(x)))),Num(10)), "Test failed! parse(\"{with {y 10} {fun {x} {+ y x}}}\") == App(Fun(Id(y),Fun(Id(x),Add(Id(y),Id(x)))),Num(10))")

    // numOperator
    assert(numOperator(_+_) (NumV(1), NumV(2)) == NumV(3), "Test failed! numOperator(_+_) (NumV(1), NumV(2)) == NumV(3)")
    assert(numOperator(_-_) (NumV(2), NumV(1)) == NumV(1), "Test failed! numOperator(_-_) (NumV(2), NumV(1)) == NumV(1)")

    // lookup
    assert(lookup(Id("x"), ASub(Id("x"), NumV(10), MtSub)) == NumV(10), "Test failed! lookup(Id(\"x\"), ASub(Id(\"x\"), NumV(10), MtSub)) == NumV(10)")
    assert(lookup(Id("y"), ASub(Id("x"), NumV(5), ASub(Id("y"), NumV(10), MtSub))) == NumV(10), "Test failed! lookup(Id(\"y\"), ASub(Id(\"x\"), NumV(5), ASub(Id(\"y\"), NumV(10), MtSub))) == NumV(10)")

    // interp
    assert(interp(parse("{+ 1 2}"), MtSub) == NumV(3), "Test failed! interp(parse(\"{+ 1 2}\"), MtSub) == NumV(3)")
    assert(interp(parse("{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}}"), MtSub) == NumV(7), "Test failed! interp(parse(\"{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}}\"), MtSub) == NumV(7)")
}