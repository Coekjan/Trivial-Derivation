package frontend.exceptions

trait ExpressionCompilationError

object ExpressionCompilationError {
  case object ExpressionLexerError extends ExpressionCompilationError {
    override def toString = "LexerError"
  }
  case object ExpressionParserError extends ExpressionCompilationError {
    override def toString = "ParserError"
  }
}
