package ch.overney.aoc2018.day2

object Part2 extends App {
  Input.Data.find { str =>
    Input.Data.exists { str2 =>
      val count = str.zip(str2).count{ case (a, b) => a != b }
      val endCondition = count == 1
      if (endCondition) {
        println(s"$str <-> $str2 have a distance of 1.")
      }
      endCondition
    }
  }
}
