package ch.overney.aoc2018.day03

import scala.collection.mutable

object Part2 extends App {
  val Pattern = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r

  val resMap: mutable.Map[(Int, Int), Int] = mutable.Map()

  Input.Data.foreach {
    case Pattern(_, xStr, yStr, width, height) =>
      val x = xStr.toInt
      val y = yStr.toInt
      for {
        xs <- 1 to width.toInt
        ys <- 1 to height.toInt
      } {
        val entry = (x + xs, y + ys)
        resMap +=  entry -> (resMap.getOrElse(entry, 0) + 1)
      }
  }

  Input.Data.find {
    case s @ Pattern(id, xStr, yStr, width, height) =>
      val x = xStr.toInt
      val y = yStr.toInt
      val entries = for {
        xs <- 1 to width.toInt
        ys <- 1 to height.toInt
      } yield {
        val entry = (x + xs, y + ys)
        resMap(entry)
      }
      val predicate = entries.sum == entries.size
      if (predicate) {
        println(s"$id does not overlap at all!")
      }
      predicate
  }
}