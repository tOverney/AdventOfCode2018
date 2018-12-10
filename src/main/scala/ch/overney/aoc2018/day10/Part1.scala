package ch.overney.aoc2018.day10

import java.util.Scanner

import scala.util.matching.Regex

class MovingPoint(posX: Int, posY: Int, velX: Int, velY: Int) {
  var nextX: Int = posX
  var nextY: Int = posY
  def next(): (Int, Int) = {
    val toReturn = (nextX, nextY)
    nextX += velX
    nextY += velY
    toReturn
  }
}
object MovingPoint {
  val StrParser: Regex = """position=< ?(-?\d+),  ?(-?\d+)> velocity=< ?(-?\d+),  ?(-?\d+)>""".r
  def strToInt(str: String): Int = if (str.startsWith("-")) {
    - str.tail.toInt
  } else {
    str.toInt
  }

  def apply(str: String): MovingPoint = {
    val StrParser(x, y, vX, vY) = str
    new MovingPoint(strToInt(x), strToInt(y), strToInt(vX), strToInt(vY))
  }
}

object Part1 extends App {
  val Neighbors: Set[(Int, Int)] = Set((1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1))
  def neighbors(pos: (Int, Int)): Set[(Int, Int)] = {
    Neighbors.map { case (dx, dy) => (pos._1 + dx, pos._2 + dy)}
  }

  private val movingPoints: Array[MovingPoint] = Input.Lines(1).map(MovingPoint(_))

  val scanner = new Scanner(System.in)

  Stream.from(0).find { iter =>
    if (iter % 100 == 0) println(s"Iter n°$iter")

    val points = movingPoints.map(_.next()).toSet

    def notAlone(pos: (Int, Int)): Boolean = {
      neighbors(pos).exists(points)
    }

    val predicate = points.forall(notAlone)

    if (predicate) {
      println(s"Found at $iter")
      val minX = points.minBy(_._1)._1
      val maxX = points.maxBy(_._1)._1
      val minY = points.minBy(_._2)._2
      val maxY = points.maxBy(_._2)._2
      for {
        y <- minY to maxY
        x <- minX to maxX
      } {
        if (points((x, y))) {
          print("#")
        } else {
          print(".")
        }
        if (x == maxX) {
          println()
        }
      }
    }
    predicate
  }
}
