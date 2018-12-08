package ch.overney.aoc2018.day8

object Part1 extends App {
  val data = Input.Lines.head.split(" ").toList.map(_.toInt)

  def loop(toHandle: List[Int], acc: Int = 0, children: List[(Int, Int)] = Nil): Int = {
    lazy val first :: second :: tail = toHandle

    def removeOneChild(cs: List[(Int, Int)]): List[(Int, Int)] = if (cs.isEmpty) {
      Nil
    } else {
      val newHead = (cs.head._1 - 1, cs.head._2)
      newHead :: cs.tail
    }

    lazy val (headCount, skipdo) :: otherChildren = children
    lazy val tup = (first, second)

    //println(children + "  " + acc)
    if (toHandle.isEmpty && children.isEmpty) {
      acc
    } else if (children.isEmpty) {
      loop(tail, acc, tup :: children)
    } else if (headCount == 0) {
      loop(toHandle.drop(skipdo), acc + toHandle.take(skipdo).sum, removeOneChild(otherChildren))
    } else if (first == 0) {
      loop(tail.drop(second), acc + tail.take(second).sum, removeOneChild(children))
    } else {
      loop(tail, acc, tup :: children)
    }
  }

  println(loop(data))
}
