package frontend

import frontend.exceptions.ExpressionCompilationError.ExpressionLexerError

import scala.util.parsing.combinator.RegexParsers

object ExpressionLexer extends RegexParsers {
  sealed trait ExpressionToken
  case object WHITE_CHAR extends ExpressionToken
  case object PLUS extends ExpressionToken
  case object MINUS extends ExpressionToken
  case object MULT extends ExpressionToken
  case object LPAREN extends ExpressionToken
  case object RPAREN extends ExpressionToken
  case object SIN extends ExpressionToken
  case object COS extends ExpressionToken
  case object SYMBOL extends ExpressionToken
  case object EXPONENT extends ExpressionToken
  case class INTEGER(value: BigInt) extends ExpressionToken

  private def whiteChar = """[\t ]""".r ^^^ WHITE_CHAR
  private def plus = """\+""".r ^^^ PLUS
  private def minus = """-""".r ^^^ MINUS
  private def exponent = """\*{2}""".r ^^^ EXPONENT
  private def mult = """\*""".r ^^^ MULT
  private def lparen = """\(""".r ^^^ LPAREN
  private def rparen = """\)""".r ^^^ RPAREN
  private def sin = """sin""".r ^^^ SIN
  private def cos = """cos""".r ^^^ COS
  private def symbol = """x""".r ^^^ SYMBOL
  private def integer = """[0-9]+""".r ^^ { s => INTEGER(BigInt(s)) }

  private def tokens = phrase(
    rep1(whiteChar | plus | minus | exponent | mult | lparen | rparen | sin | cos | symbol | integer)
  )

  def apply(string: String) = parse(tokens, string) match {
    case NoSuccess(_, _) => Left(ExpressionLexerError)
    case Success(result, _) => Right(result)
  }
}
