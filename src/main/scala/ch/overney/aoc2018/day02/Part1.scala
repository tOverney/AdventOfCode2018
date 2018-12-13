package ch.overney.aoc2018.day02

object Part1 extends App {
  def incrementIf(count: Int, predicate: Boolean): Int = if (predicate) count + 1 else count

  val (firstPartCheckSum, secondPartChecksum) = Input.Data.foldLeft((0, 0)) { case ((sum2, sum3), str) =>
    val (_, exact2, exact3, _) = str.foldLeft(Set[Char](), Set[Char](), Set[Char](), Set[Char]()) {
      case (acc @ (ones, twos, threes, mores), currChar) =>
        if (ones(currChar)) {
          (ones - currChar, twos + currChar, threes, mores)
        } else if (twos(currChar)) {
          (ones, twos - currChar, threes + currChar, mores)
        } else if (threes(currChar)) {
          (ones, twos, threes - currChar, mores + currChar)
        } else if (mores(currChar)) {
          acc
        } else {
          (ones + currChar, twos, threes, mores)
        }
    }

    (incrementIf(sum2, exact2.nonEmpty), incrementIf(sum3, exact3.nonEmpty))
  }

  println(s"$firstPartCheckSum * $secondPartChecksum = " + (firstPartCheckSum * secondPartChecksum))
}
