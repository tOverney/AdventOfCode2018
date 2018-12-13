package ch.overney.aoc2018.day05

object Part2 extends App {
  @scala.annotation.tailrec
  def helper(entry: String, acc: String, tail: String, lastChar: Option[Char]): String = {
    lazy val lastCharUpper = lastChar.map(_.toUpper)
    if (tail.isEmpty) {
      val result: String = acc ++ lastChar
      if (entry != result) {
        helper(result, "", result.tail, result.headOption)
      } else {
        result
      }
    } else if (lastChar.contains(tail.head)) {
      helper(entry, acc ++ lastChar, tail.tail, lastChar)
    } else if (lastCharUpper.contains(tail.head.toUpper)) {
      val tl = tail.tail
      val (headOfTail, tailOfTail) = if (tl.length <= 1) (None, "") else (tl.headOption, tl.tail)
      if (acc.isEmpty) {
        helper(entry, acc, tailOfTail, headOfTail)
      } else {
        helper(entry, acc.dropRight(1), tl, acc.takeRight(1).headOption)
      }
    } else {
      helper(entry, acc ++ lastChar, tail.tail, tail.headOption)
    }
  }

  def caller(input: String): String = {
    helper(input, "", input.tail, input.headOption)
  }

  val res = for {
    i <- 65 to 90
  } yield {
    val c = i.toChar
    val reduced = Input.rawData.filterNot(_.toUpper == c)
    val res = caller(reduced)
    println(i.toChar + "   " + res.length)
    (c, res.length)
  }

  println(res.minBy(_._2))
}
