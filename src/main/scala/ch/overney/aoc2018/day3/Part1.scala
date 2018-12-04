package ch.overney.aoc2018.day3

import scala.collection.mutable

object Part1 extends App {
  val Pattern = """#\d+ @ (\d+),(\d+): (\d+)x(\d+)""".r

  val resMap: mutable.Map[(Int, Int), Int] = mutable.Map()

  Input.Data.foreach {
    case s @ Pattern(xStr, yStr, width, height) =>
      println(s + "  " + width + " ** " + height)
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

  val overlaps = resMap.count(_._2 > 1)

  println(s"There are $overlaps overlaps!")
}
