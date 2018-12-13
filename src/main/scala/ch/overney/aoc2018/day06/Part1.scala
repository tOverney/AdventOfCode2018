package ch.overney.aoc2018.day06

import scala.collection.mutable

object Part1 extends App {
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
    x <- minX to maxX
    y <- minY to maxY
  } yield {
    val distanced = points.map(tup => (tup, manDistance(x, y, tup._1)))
//    println(x + "  " + y + "  " + distanced.unzip._2.toList)
    val closests = distanced.groupBy(_._2).minBy(_._1)._2
    val closest = if (closests.length != 1) -1 else closests.head._1._2

    if (Set(minX, maxX)(x) || Set(minY, maxY)(y)) {
     invalids += closest
    }
    (List(x, y), closest)
  }
  println(invalids)

  val res = ids.filterNot(tup => invalids(tup._2)).groupBy(_._2).toList.map(tup => (tup._1, tup._2.size))
  println(res.sortBy(-_._2).mkString("\n"))

}
