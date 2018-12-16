package ch.overney.aoc2018.day16

object Part1 extends App {
  val RegStatePattern = """(Before|After)\: +\[(\d), (\d), (\d), (\d)\]""".r
  val MysteryOpPattern = """\d+ (\d+) (\d+) (\d)""".r

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
      initial.updated(out, initial(a) + b)
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

  val res = Input.DataEntries(1).sliding(4, 4).count {
    case RegStatePattern(_, a, b, c, d) :: MysteryOpPattern(e, f, g) :: RegStatePattern(_, h, i, j, k) :: _ :: Nil =>
      val inA = e.toInt
      val inB = f.toInt
      val out = g.toInt
      val regInit = Vector(a.toInt, b.toInt, c.toInt, d.toInt)
      val regRes = Vector(h.toInt, i.toInt, j.toInt, k.toInt)
      OPCode.values.count(_.run(inA, inB, out, regInit) == regRes) >= 3
  }
  println(res)
}
