package frontend

import core.Derivable
import core.Derivable.{Add, Con, Cos, Mul, Pow, Sin, Var}
import frontend.ExpressionLexer.{COS, EXPONENT, INTEGER, LPAREN, MINUS, MULT, PLUS, RPAREN, SIN, SYMBOL, WHITE_CHAR}
import frontend.exceptions.ExpressionCompilationError.ExpressionParserError

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Reader}

object ExpressionParser extends Parsers {
  override type Elem = ExpressionLexer.ExpressionToken

  private class ExpressionTokenReader(tokens: Seq[Elem]) extends Reader[Elem] {
    override def first = tokens.head

    override def rest = new ExpressionTokenReader(tokens.tail)

    override def pos = NoPosition

    override def atEnd = tokens.isEmpty
  }

  private val whites = rep(WHITE_CHAR)
  private val plusMinus = PLUS | MINUS

  private def expression: ExpressionParser.Parser[Add] =
    phrase(whites ~ opt(plusMinus ~ whites) ~ term ~ rep(whites ~ plusMinus ~ whites ~ term) ~ whites) ^^ {
      case _ ~ sign ~ term ~ list ~ _ => sign match {
        case Some(MINUS ~ _) => Add((-1 * term) :: list.map(_._2))
        case _ => Add(term :: list.map(_._2))
      }
    }
  private def term: ExpressionParser.Parser[Mul] =
    (opt(plusMinus ~ whites) ~ factor ~ rep(whites ~ MULT ~ whites ~ factor)) ^^ {
      case sign ~ factor ~ list => sign match {
        case Some(MINUS ~ _) => Mul(-1 :: factor :: list.map(_._2))
        case _ => Mul(factor :: list.map(_._2))
      }
    }
  private def factor: ExpressionParser.Parser[Derivable] = (powFactor | triFactor | conFactor | expFactor) ^^ (f => f)
  private def conFactor: ExpressionParser.Parser[Con] = signedInt ^^ (int => Con(int.value))
  private def expFactor: ExpressionParser.Parser[Add] = (LPAREN ~ expression ~ RPAREN) ^^ {
    case _ ~ expression ~ _ => expression
  }
  private def triFactor: ExpressionParser.Parser[Derivable] =
    ((SIN | COS) ~ whites ~ LPAREN ~ whites ~ expression ~ whites ~ RPAREN ~ opt(whites ~ index)) ^^ {
      case SIN ~ _ ~ _ ~ _ ~ factor ~ _ ~ _ ~ exp => exp match {
        case Some(_ ~ exp) => Sin(factor) ~^ exp.value.intValue
        case None => Sin(factor)
      }
      case COS ~ _ ~ _ ~ _ ~ factor ~ _ ~ _ ~ exp => exp match {
        case Some(_ ~ exp) => Cos(factor) ~^ exp.value.intValue
        case None => Cos(factor)
      }
    }
  private def powFactor: ExpressionParser.Parser[Derivable] = (SYMBOL ~ opt(whites ~ index)) ^^ {
    case _ ~ Some(_ ~ exp) => Pow(Var("x"), exp.value.intValue)
    case _ => Var("x")
  }
  private def index: ExpressionParser.Parser[INTEGER] = (EXPONENT ~ whites ~ signedInt) ^^ {
    case _ ~ _ ~ int => int
  }
  private def signedInt: ExpressionParser.Parser[INTEGER] = (opt(plusMinus) ~ integer) ^^ {
    case Some(MINUS) ~ integer => INTEGER(-integer.value)
    case _ ~ integer => integer
  }
  private def integer: ExpressionParser.Parser[INTEGER] = accept("integer", {
    case integer: INTEGER => integer
  })

  def apply(tokens: Seq[ExpressionLexer.ExpressionToken]) = {
    expression(new ExpressionTokenReader(tokens)) match {
      case NoSuccess(_, _) => Left(ExpressionParserError)
      case Success(result, _) => Right(result)
    }
  }
}
