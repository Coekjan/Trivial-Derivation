package core

import core.Derivable.{Cos, Sin, Var}

import sys.process._
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer

class DerivableTest extends AnyFunSuite {
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
    (x * (x + 1) * x ~^ 4) * (x * Sin(x + x ~^ 4))
  )

  def derivationTest(derivable: Derivable): (Derivable, Boolean, String) = {
    val out = new ListBuffer[String]
    val err = new ListBuffer[String]
    val derivationResult = derivable.derive
    val cmd = s"$DERIVATION_TEST_SYMPY_PROC '${derivable.toString}' '${derivationResult.toString}'"
    val exitCode = cmd!ProcessLogger(out += _, err += _)
    (derivationResult, exitCode == 0 && err.isEmpty && out.head == "True", out.last)
  }

  def simplificationTest(derivable: Derivable): (Derivable, Boolean) = {
    val out = new ListBuffer[String]
    val err = new ListBuffer[String]
    val simplificationResult = derivable.simplify
    val cmd = s"$SIMPLIFICATION_TEST_SYMPY_PROC '${derivable.toString}' '${simplificationResult.toString}'"
    val exitCode = cmd!ProcessLogger(out += _, err += _)
    (simplificationResult, exitCode == 0 && err.isEmpty && out.head == "True")
  }

  test("core.Derivable::derive") {
    val failed = testSet.map(d => {
      val judge = derivationTest(d)
      (d, judge._1, judge._2, judge._3)
    }).filter(!_._3).map {
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
    val failed = testSet.flatMap(d => List(d, d.derive)).map(d => {
      val judge = simplificationTest(d)
      (d, judge._1, judge._2)
    }).filter(!_._3).map {
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
