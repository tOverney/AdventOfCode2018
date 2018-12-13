package ch.overney.aoc2018.day08

object Part2 extends App {
  val data = Input.Lines.head.split(" ").toList.map(_.toInt)

  case class Level(totChildren: Int, metatDataSize: Int, processedChildren: Int, childrenAcc: List[Int]) {
    def addChildResult(value: Int): Level = this.copy(childrenAcc = childrenAcc :+ value)
    def withOneProcessed: Level = this.copy(processedChildren = processedChildren + 1)
    def isAllProcessed: Boolean = totChildren == processedChildren

    def processMetaData(rawData: List[Int]): (Int, List[Int]) = {
      val (md, tail) = rawData.splitAt(metatDataSize)
      val nodeValue = md.map(childId => childrenAcc.applyOrElse(childId - 1, (_: Int) => 0)).sum

      (nodeValue, tail)
    }
  }

  def loop(toHandle: List[Int], acc: Int = 0, children: List[Level] = Nil): Int = {
    lazy val first :: second :: tail = toHandle

    def removeOneChild(cs: List[Level], value: Int): List[Level] =
      if (cs.isEmpty) Nil else cs.head.withOneProcessed.addChildResult(value) :: cs.tail

    lazy val currLevel :: otherChildrenLevel = children
    lazy val tup = Level(first, second, 0, Nil)

    // println(children + "  " + acc)
    if (toHandle.isEmpty && children.isEmpty) {
      acc
    } else if (children.isEmpty) {
      loop(tail, acc, tup :: children)
    } else if (currLevel.isAllProcessed) {
      val (nodeValue, remainToHandle) = currLevel.processMetaData(toHandle)
      val newAcc = acc + (if (otherChildrenLevel.isEmpty) nodeValue else 0)
      loop(remainToHandle, newAcc, removeOneChild(otherChildrenLevel, nodeValue))
    } else if (first == 0) {
      val updatedChildren = removeOneChild(children, tail.take(second).sum)
      loop(tail.drop(second), acc, updatedChildren)
    } else {
      loop(tail, acc, tup :: children)
    }
  }

  println(loop(data))
}
