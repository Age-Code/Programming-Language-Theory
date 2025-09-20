import $ivy.`org.scala-lang.modules::scala-parser-combinators:2.4.0` // Used by Ammonite
import scala.util.parsing.combinator._

trait Expr
case class Num(num: Int) extends Expr
case class Add(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr) extends Expr

// Custom exception class with no stack trace
class SimpleException(message: String) extends RuntimeException(message) {
  override def fillInStackTrace(): Throwable = this
  override def printStackTrace(): Unit = println(message)
}

def parse(input: String): Expr = {
  object Parser extends RegexParsers {
    def int: Parser[Int] = """\d+""".r ^^ { _.toInt }
    def wrap[T](parser: Parser[T]): Parser[T] = "{" ~> parser <~ "}"
    
    lazy val expr: Parser[Expr] =
      int ^^ { case n => Num(n) } | 
        wrap("+" ~> expr ~ expr) ^^ { case l ~ r => Add(l, r) } |
        wrap("-" ~> expr ~ expr) ^^ { case l ~ r => Sub(l, r) }
    
    def parseAllExpr(input: String): Expr =
      parseAll(expr, input).getOrElse(throw new SimpleException(s"bad syntax: $input"))
  }
  Parser.parseAllExpr(input)
}

// [contract] interp : Expr => Int
def interp(expr: Expr): Int = expr match {
  case Num(n) => n
  case Add(l, r) => interp(l) + interp(r)
  case Sub(l, r) => interp(l) - interp(r)
}

// main
@main def run(): Unit = {
  
  // This, "{+ 3 {- 8 2}}, is code written in our toy language!
  val codeInConcreteSyntax = "{+ 3 {- 8 2}}"
  
  // Parse the code to convert to abstract syntax
  val abstracSytaxOfCode = parse(codeInConcreteSyntax)
  
  // It leads to 9 as a result.
  println(interp(abstracSytaxOfCode))
  
  // println(parse("3"))
  // println(parse("{+ 3 4}"))
  println(parse("{+ {- 3 4} 7}")) // ((3 - 4) + 7)
  // println(parse("{- 5 1 2}"))
  
  assert(interp(parse("3")) == 3, "Test failed! interp(parse(\"3\")) == 3")
  assert(interp(parse("{+ 3 4}")) == 7, "Test failed! interp(parse(\"{+ 3 4}\")) == 7")
  assert(interp(parse("{+ {- 3 4} 7}")) == 6, "Test failed! interp(parse(\"{+ {- 3 4} 7}\")) == 6")
}