package ch.overney.aoc2018.day7

import scala.collection.mutable

object Part2 extends App {
  val Extractor = """Step ([A-Z]) must be finished before step ([A-Z]) can begin.""".r

  val nbWorker = 5
  val flatTime = 60
  val baseValue: Int = flatTime - 'A'.toInt + 1

  val workers = Array.fill(nbWorker)((-1, 'a'))
  val resMap = mutable.Map[Char, Set[Char]]()
  Input.Lines.foreach {
    case Extractor(dep, curr) =>
      val charCurr = curr.head
      val charDep = dep.head
      val currDep = resMap.getOrElse(charCurr, Set())

      resMap += charCurr -> currDep.+(charDep)
      if (!resMap.contains(charDep)) {
        resMap += charDep -> Set()
      }
  }

  val removed = mutable.Set[Char]()
  var timeElapsed = 0

  while(resMap.nonEmpty) {
    val (completedTasks, availableWorker) = workers.zipWithIndex.filter(_._1._1 <= 0).unzip
    completedTasks.sortBy(_._2).foreach {
      case (-1, _) => // Do nothing
      case (_, char) =>
        removed += char
    }
    if (resMap.nonEmpty) {
      val readyTasks = resMap.filter(deps => (deps._2 -- removed).isEmpty).toList.sortBy(_._1)
      availableWorker.zip(readyTasks).foreach { case (id, (char, _)) =>
        workers(id) = (baseValue + char, char)
        resMap -= char
      }
      workers.zipWithIndex.foreach {
        case ((-1 | 0, _), _)   => // do nothing
        case ((time, char), id) => workers(id) = (time - 1, char)
      }
      timeElapsed += 1
    }
  }

  timeElapsed += workers.maxBy(_._1)._1
  println(timeElapsed)
}
