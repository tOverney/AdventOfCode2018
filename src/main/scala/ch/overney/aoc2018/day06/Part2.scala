package ch.overney.aoc2018.day06

import scala.collection.mutable

object Part2 extends App {
  val coordinates = Input.Lines.map(_.split(", ").toList.map(_.toInt))
  val minX = coordinates.minBy(_.head).head
  val maxX = coordinates.maxBy(_.head).head
  val minY = coordinates.minBy(_(1)).apply(1)
  val maxY = coordinates.maxBy(_(1)).apply(1)
  println(minX + "  " + maxX + "  " + minY + "  " + maxY)

  val points = coordinates.zipWithIndex
  val invalids = mutable.Set[Int](-1)

  def manDistance(x: Int, y: Int, xy: List[Int]): Int = {
    val List(currX, currY) = xy
    Math.abs(x - currX) + Math.abs(y - currY)
  }

  val ids = for {
    x <- 0 to maxX + 10
    y <- 0 to maxY + 10
  } yield {
    val distanced = points.map(tup => manDistance(x, y, tup._1))
    distanced.sum
  }
  println(invalids)

  println(ids.count(_ < 10000))

}
