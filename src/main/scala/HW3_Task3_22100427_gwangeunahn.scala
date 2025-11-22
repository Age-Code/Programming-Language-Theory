// (2 points) Implement RCFAE in Scala as in L16. Submit your scala file in LMS
// Solved by myself: Y
// (I referred to the course materials)
// 06-07-08-Substitution, 09-10-Functions, 11-Deferring Substitution,
// 12-13-14-First-Class Functions, 15-Recursion, 16-Implementing Recursion
// Time taken: about 122 mins

import $ivy.`org.scala-lang.modules::scala-parser-combinators:2.4.0` // Used by Ammonite
import scala.util.parsing.combinator._

trait Expr
case class Num(n: Int) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Sub(lhs: Expr, rhs: Expr) extends Expr
case class Mul(lhs: Expr, rhs: Expr) extends Expr
case class Id(name: String) extends Expr
case class Fun(param: Id, body: Expr) extends Expr
case class App(funExpr: Expr, argExpr: Expr) extends Expr
case class If0(testExpr: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr
case class Rec(fname: Id, namedExpr: Expr, fstCall: Expr) extends Expr

class SimpleException(message: String) extends RuntimeException(message) {
  override def fillInStackTrace(): Throwable = this
  override def printStackTrace(): Unit = println(message)
}

// [contract] parse: String -> Expr
// [purpose] To convert a concrete RCFAE syntax string into an Expr abstract syntax tree.
// [tests] parse("{with {x 3} {+ x x}}") => App(Fun(Id("x"), Add(Id("x"), Id("x"))), Num(3))
//         parse("{rec {count {fun {n} {if0 n 0 {+ 1 {count {- n 1}}}}}} {count 8}}")
//         => Rec(Id("count"), Fun(Id("n"), If0(Id("n"), Num(0), Add(Num(1), App(Id("count"), Sub(Id("n"), Num(1)))))), App(Id("count"), Num(8)))
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
        wrap("*" ~> expr ~ expr) ^^ { case l ~ r => Mul(l, r) } |
        wrap("if0" ~> expr ~ expr ~ expr) ^^ {
            case testExpr ~ thenExpr ~ elseExpr =>
            If0(testExpr, thenExpr, elseExpr)
        } |
        wrap("rec" ~> wrap(symbol ~ expr) ~ expr) ^^ {
            case (Id(name) ~ namedExpr) ~ fstCall =>
            Rec(Id(name), namedExpr, fstCall)
        } |
        wrap("with" ~> wrap(symbol ~ expr) ~ expr) ^^ {
            case (Id(name) ~ value) ~ body =>
            App(Fun(Id(name), body), value)
        } |
        wrap("fun" ~> wrap(symbol) ~ expr) ^^ {
            case Id(param) ~ body =>
            Fun(Id(param), body)
        } |
        wrap(expr ~ expr) ^^ {
            case ftn ~ arg => App(ftn, arg)
        }

        def parseAllExpr(str: String): Expr =
            parseAll(expr, str).getOrElse(throw new SimpleException(s"bad syntax: $str"))
    }
    Parser.parseAllExpr(input)
}

trait DefrdSub
case object MtSub extends DefrdSub
case class ASub(name: Id, value: ExprValue, ds: DefrdSub) extends DefrdSub
case class ARecSub(fname: Id, var valueBox: ExprValue, ds: DefrdSub) extends DefrdSub

trait ExprValue
case class NumV(n: Int) extends ExprValue
case class ClosureV(param: Id, body: Expr, ds: DefrdSub) extends ExprValue

// [contract] lookup: (Id, DefrdSub) -> ExprValue
// [purpose] To find the value of an identifier in the deferred substitution environment.
// [tests] lookup(Id("x"), ASub(Id("x"), NumV(10), MtSub)) => NumV(10)
//         lookup(Id("count"), ARecSub(Id("count"), NumV(1), MtSub)) => NumV(1)
def lookup(name: Id, ds: DefrdSub): ExprValue = ds match {
    case MtSub =>
        throw new Exception(s"Free identifier: $name")
    case ASub(id, v, saved) =>
        if (id == name) v else lookup(name, saved)
    case ARecSub(fname, valueBox, restds) =>
        if (fname == name) valueBox else lookup(name, restds)
}

// [contract] numzero: ExprValue -> Boolean
// [purpose] To check whether a numeric value is zero (used by if0).
// [tests] numzero(NumV(0)) => true
//         numzero(NumV(3)) => false
def numzero(n: ExprValue): Boolean = n match {
    case NumV(x) => x == 0
}

// [contract] numOperator: ((Int, Int) => Int) -> ((ExprValue, ExprValue) => ExprValue)
// [purpose] To lift a primitive numeric operator on Int to work on NumV values.
// [tests] numOperator(_+_)(NumV(1), NumV(2)) => NumV(3)
//         numOperator(_-_)(NumV(2), NumV(1)) => NumV(1)
//         numOperator(_*_)(NumV(2), NumV(1)) => NumV(2)
def numOperator(op: (Int, Int) => Int): (ExprValue, ExprValue) => ExprValue = {
    case (NumV(x), NumV(y)) => NumV(op(x, y))
    case _ => throw new SimpleException("Expected numerical expressions")
}

val numAdd: (ExprValue, ExprValue) => ExprValue = numOperator(_ + _)
val numSub: (ExprValue, ExprValue) => ExprValue = numOperator(_ - _)
val numMul: (ExprValue, ExprValue) => ExprValue = numOperator(_ * _)

// [contract] interp: (Expr, DefrdSub) -> ExprValue
// [purpose] To evaluate an RCFAE expression in the given deferred substitution environment.
// [tests] interp(parse("{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}}"), MtSub) => NumV(7)
//         interp(parse("{rec {count {fun {n} {if0 n 0 {+ 1 {count {- n 1}}}}}} {count 8}}"), MtSub) => NumV(8)
def interp(expr: Expr, ds: DefrdSub): ExprValue = expr match {
    case Num(n) => NumV(n)
    case Add(l, r) => numAdd(interp(l, ds), interp(r, ds))
    case Sub(l, r) => numSub(interp(l, ds), interp(r, ds))
    case Mul(l, r) => numMul(interp(l, ds), interp(r, ds))
    case Id(name) => lookup(Id(name), ds)
    case Fun(p, b) => ClosureV(p, b, ds)
    case App(ftn, arg) =>
        val fVal = interp(ftn, ds)
        val aVal = interp(arg, ds)
        fVal match {
        case ClosureV(param, body, closure_ds) =>
            interp(body, ASub(param, aVal, closure_ds))
        case _ =>
            throw new SimpleException("Expected a function")
        }
    case If0(testExpr, thenExpr, elseExpr) =>
        if (numzero(interp(testExpr, ds)))
        interp(thenExpr, ds)
        else
        interp(elseExpr, ds)
    case Rec(fname, namedExpr, fstCall) =>
        var valueHolder: ExprValue = NumV(198)
        var newDs = ARecSub(fname, valueHolder, ds)
        valueHolder = interp(namedExpr, newDs)
        newDs.valueBox = valueHolder
        interp(fstCall, newDs)
}

// main
@main def run(): Unit = {

    // parse tests
    assert(
        parse("{with {x 3} {+ x x}}") ==
        App(Fun(Id("x"), Add(Id("x"), Id("x"))), Num(3)),
        "Test failed! parse(\"{with {x 3} {+ x x}}\") == App(Fun(Id(\"x\"), Add(Id(\"x\"), Id(\"x\"))), Num(3))"
    )

    val countProg = "{rec {count {fun {n} {if0 n 0 {+ 1 {count {- n 1}}}}}} {count 8}}"
    parse(countProg)
    assert(parse(countProg) ==
        Rec(Id("count"), Fun(Id("n"), If0(Id("n"), Num(0), Add(Num(1), App(Id("count"), Sub(Id("n"), Num(1)))))), App(Id("count"), Num(8))),
        "Test failed! parse(\"{rec {count {fun {n} {if0 n 0 {+ 1 {count {- n 1}}}}}} {count 8}}\") == Rec(Id(\"count\"), Fun(Id(\"n\"), If0(Id(\"n\"), Num(0), Add(Num(1), App(Id(\"count\"), Sub(Id(\"n\"), Num(1)))))), App(Id(\"count\"), Num(8)))"
    )

    // numOperator
    assert(
        numOperator(_+_)(NumV(1), NumV(2)) == NumV(3),
        "Test failed! numOperator(_+_)(NumV(1), NumV(2)) == NumV(3)"
    )
    assert(
        numOperator(_-_)(NumV(2), NumV(1)) == NumV(1),
        "Test failed! numOperator(_-_)(NumV(2), NumV(1)) == NumV(1)"
    )
    assert(
        numOperator(_*_)(NumV(2), NumV(1)) == NumV(2),
        "Test failed! numOperator(_*_)(NumV(2), NumV(1)) == NumV(2)"
    )

    // numzero
    assert(
        numzero(NumV(0)),
        "Test failed! numzero(NumV(0)"
    )
    assert(
        !numzero(NumV(3)),
        "Test failed! !numzero(NumV(3)"
    )

    // lookup
    assert(
        lookup(Id("x"), ASub(Id("x"), NumV(10), MtSub)) == NumV(10),
        "Test failed! lookup(Id(\"x\"), ASub(Id(\"x\"), NumV(10), MtSub)) == NumV(10)"
    )
    assert(
        lookup(Id("count"), ARecSub(Id("count"), NumV(1), MtSub)) == NumV(1),
        "Test failed! lookup(Id(\"count\"), ARecSub(Id(\"count\"), NumV(1), MtSub)) == NumV(1)"
    )

    // interp
    val withClosureProg =
        "{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}}"
    assert(
        interp(parse(withClosureProg), MtSub) == NumV(7),
        "Test failed! interp(parse(\"{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}}\"), MtSub) == NumV(7)"
    )
    assert(
        interp(parse(countProg), MtSub) == NumV(8),
        "Test failed! interp(parse(\"{rec {count {fun {n} {if0 n 0 {+ 1 {count {- n 1}}}}}} {count 8}}\"), MtSub) == NumV(8)"
    )
}