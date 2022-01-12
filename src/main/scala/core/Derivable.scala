package core

import core.Derivable.{Add, Con, Cos, Mul, Pow, Sin, Var}

import scala.language.implicitConversions
import scala.math.{BigInt, pow}

trait Derivable extends Ordered[Derivable] {
  private val order: Derivable => Int = d => List(
    classOf[Con],
    classOf[Var],
    classOf[Pow],
    classOf[Sin],
    classOf[Cos],
    classOf[Mul],
    classOf[Add]
  ).indexOf(d.getClass)

  def derive: Derivable
  def stringify: String
  def simplify: Derivable
  def extractPow: Pow = Pow(this, 1)
  def extractCoe: (Con, Pow) = (1, Pow(this, 1))

  override def toString = stringify

  def +(right: Derivable): Derivable = (this, right) match {
    case (Con(left), Con(right)) => Con(left + right)
    case (Add(left), Add(right)) => Add(left ::: right)
    case (Add(left), derivable) => Add(left :+ derivable)
    case (derivable, Add(right)) => Add(derivable :: right)
    case (left, right) => Add(left :: right :: Nil)
  }

  def *(right: Derivable): Derivable = (this, right) match {
    case (Con(left), Con(right)) => Con(left * right)
    case (Mul(left), Mul(right)) => Mul(left ::: right)
    case (Mul(left), derivable) => Mul(left :+ derivable)
    case (derivable, Mul(right)) => Mul(derivable :: right)
    case (left, right) => Mul(left :: right :: Nil)
  }

  def ~^(con: Con): Derivable = this match {
    case Con(c) => Con(pow(c.toDouble, con.value.toDouble).intValue)
    case Pow(base, exp) => Pow(base, exp + con)
    case derivable => Pow(derivable, con)
  }

  override def compare(that: Derivable) = order(this) compare order(that)
}

object Derivable {
  implicit def valToCon(value: Int): Con = Con(value)
  implicit def ConToInt(con: Con): Int = con match {
    case Con(value) if value.isValidInt => value.intValue
  }
  implicit private def indexedSeqToList[T](indexedSeq: IndexedSeq[T]): List[T] = indexedSeq.toList

  case class Add(derivableList: List[Derivable]) extends Derivable {
    require(derivableList.nonEmpty)

    override def derive = Add(
      for (derivable <- derivableList) yield derivable.derive
    )

    override def stringify = derivableList.map(_.stringify).reduce(_ + "+" + _)

    override def simplify = {
      derivableList.map(_.simplify)
        .flatMap {
          case Add(list) => list
          case box => List(box)
        }
        .filter {
          case Con(value) => value != 0
          case _ => true
        }
        .map(_.extractCoe)
        .groupMapReduce(_._2)(_._1)(_ + _)
        .map {
          case (pow, con) => (con * pow.simplify).simplify
        }
        .groupMapReduce {
          case _: Con => Con(1)
          case der => der
        }{
          case Con(value) => value
          case _ => BigInt(0)
        }(_ + _)
        .map {
          case (Con(value), coe) if value == BigInt(1) => Con(coe)
          case (der, _) => der
        }.toList match {
        case Nil => 0
        case List(one) => one
        case list => Add(list.sorted)
      }
    }
  }

  case class Mul(derivableList: List[Derivable]) extends Derivable {
    require(derivableList.nonEmpty)

    override def derive = Add(
      for (i <- derivableList.indices) yield derivableList(i).derive * Mul(
        (for (j <- derivableList.indices if i != j) yield derivableList(j)).toList match {
          case Nil => List(1)
          case l => l
        }
      )
    )

    override def stringify = derivableList.map {
      case add: Add => s"(${add.stringify})"
      case box => box.stringify
    }.reduce(_ + "*" + _)

    override def simplify = {
      derivableList.map(_.simplify)
        .flatMap {
          case Mul(list) => list
          case box => List(box)
        }
        .filter {
          case Con(value) => value != 1
          case _ => true
        } match {
        case list if list.contains(Con(0)) => 0
        case list => list.map(_.extractPow)
          .groupMapReduce(_.base)(_.exp)(_ + _)
          .map {
            case (base, exp) => Pow(base, exp).simplify
          }
          .groupMapReduce {
            case _: Con => Con(1)
            case der => der
          }{
            case Con(value) => value
            case _ => BigInt(1)
          }(_ * _)
          .map {
            case (Con(value), coe) if value == BigInt(1) => Con(coe)
            case (der, _) => der
          }.toList match {
          case Nil => 1
          case List(one) => one
          case list => Mul(list.sorted)
        }
      }
    }

    override def extractCoe = derivableList match {
      case (con: Con) :: Nil => (con, Pow(1, 1))
      case (con: Con) :: one :: Nil => one match {
        case pow: Pow => (con, pow)
        case _ => (con, Pow(one, 1))
      }
      case (con: Con) :: list => (con, Pow(Mul(list), 1))
      case _ => super.extractCoe
    }
  }

  case class Sin(func: Derivable) extends Derivable {
    override def derive = func.derive * Cos(func)

    override def stringify = s"sin(${func.stringify})"

    override def simplify = func.simplify match {
      case Con(value) if value == BigInt(0) => 0
      case der => Sin(der)
    }
  }

  case class Cos(func: Derivable) extends Derivable {
    override def derive = -1 * func.derive * Sin(func)

    override def stringify = s"cos(${func.stringify})"

    override def simplify = func.simplify match {
      case Con(value) if value == BigInt(0) => 1
      case der => Cos(der)
    }
  }

  case class Pow(base: Derivable, exp: Int) extends Derivable {
    override def derive = exp * base ~^ (exp - 1) * base.derive

    override def stringify = base match {
      case Var(symbol) => s"$symbol**$exp"
      case Con(value) => s"$value**$exp"
      case _ => s"(${base.stringify})**$exp"
    }

    override def simplify = base.simplify match {
      case Con(value) => Con(value.pow(exp))
      case Pow(base, exponent) => Pow(base, exponent + exp)
      case simplifiedBase => exp match {
        case 0 => 1
        case 1 => simplifiedBase
        case _ => Pow(simplifiedBase, exp)
      }
    }

    override def extractPow = this

    override def extractCoe = (1, this)
  }

  case class Var(symbol: String) extends Derivable {
    override def derive = 1

    override def stringify = symbol

    override def simplify = this
  }

  case class Con(value: BigInt) extends Derivable {
    def +(con: Con): Con = Con(value + con.value)
    def -(con: Con): Con = Con(value - con.value)

    override def derive = 0

    override def stringify = value.toString

    override def simplify = this
  }
}
