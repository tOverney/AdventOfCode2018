package ch.overney.aoc2018.day12

object Part1 extends App {
  val PatternParser = """([#\.]{5}) => (#|\.)""".r
  val initial :: _ :: patterns = Input.DataEntries(1).toList
  val maxGen = 1000

  def stred(cells: Set[Int], start: Int = 0, stop: Int = 4): String = {
    (start to stop).map { cellId =>
      if (cells(cellId)) '#'
      else '.'
    }.mkString
  }

  def printState(state: Set[Int]): Unit = {
    val min = state.min - 2
    val max = state.max + 2
    println(stred(state, min, max))
  }

  val initialState = initial.drop(15).zipWithIndex.collect { case ('#', idx) => idx }.toSet
  val patternBook = patterns.map { case PatternParser(pattern, res) => pattern -> (res == "#") }.toMap
  println(patternBook)

  val finalState = (1L to maxGen).foldLeft(initialState) { (acc, currGen) =>
    val min = acc.min - 2
    val max = acc.max + 2
    val newState = (min to max).flatMap { idx =>
      val currPattern = stred(acc, idx - 2, idx + 2)
      val willHavePlant = patternBook.getOrElse(currPattern, false)
      if (willHavePlant) {
        Some(idx)
      } else {
        None
      }
    }.toSet

    printState(newState)
    println(currGen + " -> " + newState.min + "->" + newState.sum)
    newState
  }

  val result = finalState.sum
  println(result)
}
