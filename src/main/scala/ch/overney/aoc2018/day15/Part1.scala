package ch.overney.aoc2018.day15

import scala.annotation.tailrec
import scala.collection.{GenSet, mutable}

object helper {
  type Coord = (Int, Int)
  implicit class IntTuple2(tup: (Int, Int)) {
    def x: Int = tup._1
    def y: Int = tup._2

    def neighbouringCells(invalid: GenSet[Coord]): List[Coord] = {
      for {
        (dx, dy) <- Deltas
        pos = (x + dx, y + dy)
        if !invalid(pos)
      } yield pos
    }
  }
  def manDistance(a: Coord, b: Coord): Int = {
    Math.abs(a.x - b.x) + Math.abs(a.y - b.y)
  }

  val Deltas = List((-1, 0), (0, -1), (0, 1),  (1, 0))
  val GoblinsRaceName = "Goblins"
}

import helper._

object Part1 extends App {
  case class Runner(idx: Int) {
    val walls: mutable.Set[Coord] = mutable.Set[Coord]()
    sealed trait Creature {
      val raceName: String
      val ap: Int = 3
      var hp: Int = 200
      var x: Int
      var y: Int
      var stuck: Option[List[Coord]] = None

      def isAlive: Boolean = hp > 0

      def position: Coord = (x, y)

      override def toString: String = s"${raceName.head}($x, $y, $hp)"

      def neighbouringCells: List[Coord] = {
        position.neighbouringCells(walls)
      }
    }

    case class Elf(var x: Int, var y: Int) extends Creature {
      val raceName = "Elves"
    }

    case class Goblin(var x: Int, var y: Int) extends Creature {
      val raceName: String = GoblinsRaceName
    }


    def apply(shouldPrint: Boolean, ascii: Boolean = false): String = {
      def printlnOpt(x: => Any): Unit = if (shouldPrint) println(x)
      def printOpt(x: => Any): Unit = if (shouldPrint) print(x)

      val input = Input.DataEntries(idx)
      val creatures = input.zipWithIndex.flatMap { case (row, x) =>
        row.zipWithIndex.flatMap { case (cell, y) =>
          val content: Option[Creature] = cell match {
            case '#' =>
              walls.add((x, y))
              None
            case 'G' => Some(Goblin(x, y))
            case 'E' => Some(Elf(x, y))
            case '.' | _ => None
          }

          content
        }
      }.toList

      val height = input.length
      val width = input.head.length
      val (goblins, elves) = creatures.partition(_.raceName == GoblinsRaceName)
      var roundTracker = 0
      printlnOpt(roundTracker + "   " + creatures.filter(_.isAlive).sortBy(_.position).mkString(" "))

      def printTerrain(): Unit = {
        walls.unzip
        for {
          x <- 0 until height
          y <- 0 until width
          curr = (x, y)
        } {
          if (walls(curr)) print('#')
          else {
            val cOpt = creatures.find(c => c.position == curr && c.isAlive)
            cOpt.foreach(c => print(c.raceName.head))
            cOpt.getOrElse(print('.'))
          }
          if (y == width - 1) println()
        }
      }

      def firstValidShortestPath(fromPos: Coord, targets: List[Coord], objects: Set[Coord]): Option[Coord] = {
        val newStart = System.currentTimeMillis()

        type CC = (Coord, Coord, Int)
        val visited = mutable.Set[Coord]()
        visited ++= objects

        @tailrec
        def breadthFirstTraverse(s: Stream[CC], f: CC => Stream[CC], isFinal: CC => Boolean, resAcc: List[CC]): List[CC] = {
          if (s.isEmpty) resAcc
          else if (resAcc.nonEmpty && s.head._3 > resAcc.head._3) resAcc
          else if (isFinal(s.head)) {
            visited += s.head._2
            breadthFirstTraverse(s.tail, f, isFinal, s.head :: resAcc)
          }
          else {
            visited += s.head._2
            breadthFirstTraverse(s.tail append f(s.head), f, isFinal, resAcc)
          }
        }

        def neighborsFinder(from: CC): Stream[CC] = {
          val (first, coord, distance) = from
          coord.neighbouringCells(visited).map(pos => (first, pos, distance + 1)).toStream
        }

        val initialStream = (fromPos, fromPos, 0) #:: fromPos.neighbouringCells(visited).map(x => (x, x, 1)).toStream
        val y =  breadthFirstTraverse(initialStream, neighborsFinder, node => targets.contains(node._2), Nil)
          .sortBy(_._2)
          .headOption
          .map(_._1)
        println(s"New found $y in: " + (System.currentTimeMillis() - newStart))
        y
      }

      while (goblins.exists(_.isAlive) && elves.exists(_.isAlive)) {
        val orderedForTurn = creatures.filter(_.isAlive).sortBy(_.position)

        var done = false
        orderedForTurn.foreach { c =>
          def enemies = orderedForTurn.filter(other => other.raceName != c.raceName && other.isAlive)

          if (done || !c.isAlive) {
            // continue
          } else if (enemies.isEmpty) {
            done = true
          } else {
            def punchFirstEnemyInRangeOpt = {
              val neigh = c.neighbouringCells
              enemies
                .filter(e => neigh.contains(e.position))
                .groupBy(_.hp)
                .toList
                .sortBy(_._1)
                .headOption
                .map { g =>
                  val e = g._2.head
                  e.hp -= c.ap
                  printlnOpt("Fight: " + c + " " + e)
                  e
                }
            }

            punchFirstEnemyInRangeOpt orElse {
              val objects = (creatures.collect { case x if x.isAlive => x.position } ++ walls).toSet
              val targets = enemies.flatMap(_.position.neighbouringCells(objects))

              if (targets.nonEmpty && !c.stuck.contains(creatures.map(_.position))) {
                val xyOpt = firstValidShortestPath(c.position, targets.sorted, objects)
                xyOpt.foreach { case pos @ (newX, newY) =>
                  printOpt("Move:  " + c + " ")
                  c.x = newX
                  c.y = newY
                  printlnOpt(pos)
                  if (punchFirstEnemyInRangeOpt.isEmpty && xyOpt.nonEmpty) {
                    punchFirstEnemyInRangeOpt
                  }
                }
                if (xyOpt.isEmpty) {
                  c.stuck = Some(creatures.map(_.position))
                } else {
                  c.stuck = None
                }
              }
              None
            }
          }
        }

        if (!done) {
          roundTracker += 1
        }
        if (ascii) {
          printTerrain()
        }
        printlnOpt(roundTracker + "   " + orderedForTurn.mkString(" "))
      }
      val winners = (if (elves.forall(!_.isAlive)) goblins else elves).filter(_.isAlive)

      val totalHitPoints = winners.map(_.hp).sum
      val finalScore = roundTracker * totalHitPoints

      val res = s"""Combat ends after $roundTracker full rounds
         |${winners.head.raceName} win with $totalHitPoints total hit points left
         |Outcome: $roundTracker * $totalHitPoints = $finalScore""".stripMargin
      printlnOpt(res)
      res
    }
  }

  assert(Runner(1)(false, false) == """Combat ends after 47 full rounds
                          |Goblins win with 590 total hit points left
                          |Outcome: 47 * 590 = 27730""".stripMargin, "1")

  assert(Runner(2)(false, false) == """Combat ends after 37 full rounds
                          |Elves win with 982 total hit points left
                          |Outcome: 37 * 982 = 36334""".stripMargin, "2")

  assert(Runner(3)(false) == """Combat ends after 46 full rounds
                          |Elves win with 859 total hit points left
                          |Outcome: 46 * 859 = 39514""".stripMargin, "3")

  assert(Runner(5)(false) == """Combat ends after 35 full rounds
                               |Goblins win with 793 total hit points left
                               |Outcome: 35 * 793 = 27755""".stripMargin, "5")
  assert(Runner(6)(false) == """Combat ends after 54 full rounds
                               |Goblins win with 536 total hit points left
                               |Outcome: 54 * 536 = 28944""".stripMargin, "6")
  assert(Runner(7)(false, false) == """Combat ends after 20 full rounds
                               |Goblins win with 937 total hit points left
                               |Outcome: 20 * 937 = 18740""".stripMargin, "7")

  //231264 too small
  println(Runner(Input.Data.size - 1)(true, true))
}
