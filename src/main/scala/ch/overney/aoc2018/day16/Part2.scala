package ch.overney.aoc2018.day16

object Part2 extends App {
  val RegStatePattern = """(Before|After)\: +\[(\d), (\d), (\d), (\d)\]""".r
  val MysteryOpPattern = """(\d+) (\d+) (\d+) (\d)""".r

  sealed trait OPCode {
    def run(a: Int, b: Int, out: Int, initial: Vector[Int]): Vector[Int]
    protected def bool2int(bool: Boolean): Int = if (bool) 1 else 0
  }
  case object Addr extends OPCode {
    def run(a: Int, b: Int, out: Int, initial: Vector[Int]): Vector[Int] = {
      initial.updated(out, initial(a) + initial(b))
    }
  }
  case object Addi extends OPCode {
    def run(a: Int, b: Int, out: Int, initial: Vector[Int]): Vector[Int] = {
      initial.updated(out, initial(a) + b)
    }
  }
  case object Mulr extends OPCode {
    def run(a: Int, b: Int, out: Int, initial: Vector[Int]): Vector[Int] = {
      initial.updated(out, initial(a) * initial(b))
    }
  }
  case object Muli extends OPCode {
    def run(a: Int, b: Int, out: Int, initial: Vector[Int]): Vector[Int] = {
      initial.updated(out, initial(a) * b)
    }
  }
  case object Banr extends OPCode {
    def run(a: Int, b: Int, out: Int, initial: Vector[Int]): Vector[Int] = {
      initial.updated(out, initial(a) & initial(b))
    }
  }
  case object Bani extends OPCode {
    def run(a: Int, b: Int, out: Int, initial: Vector[Int]): Vector[Int] = {
      initial.updated(out, initial(a) & b)
    }
  }
  case object Borr extends OPCode {
    def run(a: Int, b: Int, out: Int, initial: Vector[Int]): Vector[Int] = {
      initial.updated(out, initial(a) | initial(b))
    }
  }
  case object Bori extends OPCode {
    def run(a: Int, b: Int, out: Int, initial: Vector[Int]): Vector[Int] = {
      initial.updated(out, initial(a) | b)
    }
  }
  case object Setr extends OPCode {
    def run(a: Int, b: Int, out: Int, initial: Vector[Int]): Vector[Int] = {
      initial.updated(out, initial(a))
    }
  }
  case object Seti extends OPCode {
    def run(a: Int, b: Int, out: Int, initial: Vector[Int]): Vector[Int] = {
      initial.updated(out, a)
    }
  }
  case object Gtir extends OPCode {
    def run(a: Int, b: Int, out: Int, initial: Vector[Int]): Vector[Int] = {
      initial.updated(out, bool2int(a > initial(b)))
    }
  }
  case object Gtri extends OPCode {
    def run(a: Int, b: Int, out: Int, initial: Vector[Int]): Vector[Int] = {
      initial.updated(out, bool2int(initial(a) > b))
    }
  }
  case object Gtrr extends OPCode {
    def run(a: Int, b: Int, out: Int, initial: Vector[Int]): Vector[Int] = {
      initial.updated(out, bool2int(initial(a) > initial(b)))
    }
  }
  case object Eqir extends OPCode {
    def run(a: Int, b: Int, out: Int, initial: Vector[Int]): Vector[Int] = {
      initial.updated(out, bool2int(a == initial(b)))
    }
  }
  case object Eqri extends OPCode {
    def run(a: Int, b: Int, out: Int, initial: Vector[Int]): Vector[Int] = {
      initial.updated(out, bool2int(initial(a) == b))
    }
  }
  case object Eqrr extends OPCode {
    def run(a: Int, b: Int, out: Int, initial: Vector[Int]): Vector[Int] = {
      initial.updated(out, bool2int(initial(a) == initial(b)))
    }
  }
  object OPCode {
    val values = List(Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori, Setr, Seti, Gtir, Gtri, Gtrr, Eqir, Eqri, Eqrr)
  }

  val possibilities = Input.DataEntries(1).sliding(4, 4).map {
    case RegStatePattern(_, a, b, c, d) :: MysteryOpPattern(l, e, f, g) :: RegStatePattern(_, h, i, j, k) :: _ :: Nil =>
      val inA = e.toInt
      val inB = f.toInt
      val out = g.toInt
      val id = l.toInt
      val regInit = Vector(a.toInt, b.toInt, c.toInt, d.toInt)
      val regRes = Vector(h.toInt, i.toInt, j.toInt, k.toInt)
      id -> OPCode.values.filter(_.run(inA, inB, out, regInit) == regRes).toSet[OPCode]
  }

  val narrowedDown = possibilities
    .toList
    .groupBy(_._1)
    .mapValues(_.unzip._2.reduceLeft[Set[OPCode]](_ intersect _))
    .toList

  def buildMapping(toHandle: List[(Int, Set[OPCode])], mapping: Map[Int, OPCode] = Map()): Map[Int, OPCode] = {
    toHandle.sortBy(_._2.size).foldLeft(mapping, List[(Int, Set[OPCode])]()) {
      case ((acc, todo), (id, ops)) =>
        val opcodes = ops -- acc.values
        if (opcodes.size == 1) {
          (acc.updated(id, opcodes.head), todo)
        } else {
          (acc, (id, opcodes) :: todo)
        }
    } match {
      case (newMapping, Nil) => newMapping
      case (newMapping, leftOver) =>
        println(leftOver + " left to map!")
        if (toHandle.forall(leftOver.contains(_))) {
          println(mapping.mkString(" ... "))
          sys.error("Oh no!")
        }
        buildMapping(leftOver, newMapping)
    }
  }
  println(narrowedDown.mkString("\n"))
  val mapping = buildMapping(narrowedDown)
  val finalRegisters = Input.Program.foldLeft(Vector(0, 0, 0, 0)) {
    case (currentRegs, MysteryOpPattern(i, a, b, c)) =>
      val id = i.toInt
      val inA = a.toInt
      val inB = b.toInt
      val out = c.toInt
      mapping(id).run(inA, inB, out, currentRegs)
  }

  println(finalRegisters)
}