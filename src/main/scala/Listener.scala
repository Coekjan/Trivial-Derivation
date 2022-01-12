import frontend.{ExpressionLexer, ExpressionParser}

import scala.io.StdIn.readLine
import scala.sys.exit

object Listener extends App {
  Iterator.continually {
    print("~> ")
    readLine
  }.takeWhile(Option(_).nonEmpty).foreach {
    ExpressionLexer(_) match {
      case Left(err) =>
        System.err.println(err)
        exit(1)
      case Right(tokens) => ExpressionParser(tokens) match {
        case Left(err) =>
          System.err.println(err)
          exit(2)
        case Right(derivable) =>
          println(derivable.derive.simplify)
      }
    }
  }
}
