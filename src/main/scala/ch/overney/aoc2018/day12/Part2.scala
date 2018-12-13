package ch.overney.aoc2018.day12

object Part2 extends App {
  val maxGen = 50000000000L
  /**
    * These values were obtained by running Part1 to 500 iterations and analyzing results.
    */
  val sizeOfStablePattern = 63L
  val valueWhenPatternStabilize = 8717L
  val iterationWherePatternStabilize = 124L

  println(valueWhenPatternStabilize + (sizeOfStablePattern * (maxGen - iterationWherePatternStabilize)))
}
