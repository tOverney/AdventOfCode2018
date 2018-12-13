package ch.overney.aoc2018.day07

import scala.collection.mutable

object Part1 extends App {
  val Extractor = """Step ([A-Z]) must be finished before step ([A-Z]) can begin.""".r

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

  var res = ""
  val removed = mutable.Set[Char]()

  while(resMap.nonEmpty) {
    val (char, _) = resMap.filter(deps => (deps._2 -- removed).isEmpty).toList.minBy(_._1)
    res = res + char
    resMap -= char
    removed += char
  }

  println(res)
}
