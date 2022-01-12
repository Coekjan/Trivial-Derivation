package core

import core.Derivable.{Cos, Sin, Var, valToCon}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.sys.process._

class DerivableTest extends AnyFunSuite {
  private val TEST_STRENGTH = 3
  private val TEST_SYMPY_DIR = "./src/test/python"
  private val DERIVATION_TEST_SYMPY_PROC = s"python $TEST_SYMPY_DIR/derivation_test.py"
  private val SIMPLIFICATION_TEST_SYMPY_PROC = s"python $TEST_SYMPY_DIR/simplification_test.py"
  val x = Var("x")
  val testSet: List[Derivable] = List(
    x + 2 * x,
    x * 3,
    x ~^ 2,
    3 + x * x ~^ 4,
    x * x * (1 + 3 * x ~^ 4),
    Sin(x ~^ -4) * Cos(x) + Sin(Cos(4 * x)),
    x ~^ 12 * x ~^ -13 + x * Sin(x * Cos(x)),
    (x * (x + 1) * x ~^ 4) * (x * Sin(x + x ~^ 4)),
    1 + 2 + 3 * Sin(x * Cos(x)) ~^ 2 + -4 * Sin(x * Cos(x)) ~^ 2
  )

  @inline
  private def runCmd(cmd: String): (List[String], List[String], Int) = {
    val out = new ListBuffer[String]
    val err = new ListBuffer[String]
    val exitCode = cmd!ProcessLogger(out += _, err += _)
    (out.toList, err.toList, exitCode)
  }

  def derivationTest(derivable: Derivable): (Derivable, Boolean, String) = {
    val derivationResult = derivable.derive
    val (out, err, exitCode) = runCmd(
      s"$DERIVATION_TEST_SYMPY_PROC '${derivable.toString}' '${derivationResult.toString}'"
    )
    (derivationResult, exitCode == 0 && err.isEmpty && out.head == "True", out.last)
  }

  def simplificationTest(derivable: Derivable): (Derivable, Boolean) = {
    val simplificationResult = derivable.simplify
    val (out, err, exitCode) = runCmd(
      s"$SIMPLIFICATION_TEST_SYMPY_PROC '${derivable.toString}' '${simplificationResult.toString}'"
    )
    (simplificationResult, exitCode == 0 && err.isEmpty && out.head == "True")
  }

  test("core.Derivable::derive") {
    val failed = testSet.flatMap { d =>
      val list = new ListBuffer[Derivable]
      list += d
      (0 until TEST_STRENGTH).foreach(_ => list += list.last.derive)
      list
    }
      .map(d => Future {
        val judge = derivationTest(d)
        (d, judge._1, judge._2, judge._3)
      })
      .map(Await.result(_, 5.seconds))
      .filter(!_._3)
      .map {
        case (input, output, _, expect) => (input, output, expect)
      }
    failed.foreach {
      case (input, output, expect) => println(
        s"""input: $input
           |output: ${output.toString}
           |expect: $expect
           |""".stripMargin)
    }
    assert(failed.isEmpty)
  }

  test("core.Derivation::simplify") {
    val failed = testSet.flatMap { d =>
      val list = new ListBuffer[Derivable]
      list += d
      (0 until TEST_STRENGTH).foreach(_ => list += list.last.derive)
      list
    }
      .map(d => Future {
        val judge = simplificationTest(d)
        (d, judge._1, judge._2)
      })
      .map(Await.result(_, 5.seconds))
      .filter(!_._3)
      .map {
        case (input, output, _) => (input, output)
      }
    failed.foreach {
      case (input, output) => println(
        s"""input: $input
           |output: ${output.toString}
           |""".stripMargin)
    }
    assert(failed.isEmpty)
  }
}
