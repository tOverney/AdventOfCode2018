package ch.overney.aoc2018.day4

import scala.collection.mutable

object Part1 extends App {
  val DatePattern = """\[(\d+-\d+-\d+ \d+:\d+)\] .+""".r
  val StartShiftPattern = """\[\d+-\d+-\d+ \d+:\d+\] Guard #(\d+) begins shift""".r
  val WakeUpPattern = """\[\d+-(\d+-\d+) \d+:(\d+)\] wakes up""".r
  val FallsAsleep = """\[\d+-(\d+-\d+) \d+:(\d+)\] falls asleep""".r

  val resMap = mutable.Map[String, Array[Int]]()
  def newEmptyArray: Array[Int] = Array.fill(60)(0)
  def updateArray(guardId: String, from: Int, untilVal: Int): Unit = {
    val arr = resMap.getOrElse(guardId, newEmptyArray)
    for {
      m <- from until untilVal
    } {
      arr(m) += 1
    }
    resMap += guardId -> arr
  }

  val sortedData = Input.Data.sortBy { case DatePattern(date) => date }
  //println(sortedData.mkString("\n"))

  sortedData.foldLeft(("", "",  Option.empty[Int])) {
    case (_, StartShiftPattern(guardId)) =>
      (guardId, "", None)
    case ((guardId, date, minuteFellAsleep), WakeUpPattern(currDate, minuteWokenUp)) =>
      if (date == currDate && minuteFellAsleep.nonEmpty) {
        updateArray(guardId, minuteFellAsleep.get, minuteWokenUp.toInt)
        (guardId, currDate, None)
      } else if (minuteFellAsleep.nonEmpty) {
        updateArray(guardId, minuteFellAsleep.get, 60)
        (guardId, currDate, None)
      } else {
        (guardId, currDate, None)
      }
    case ((guardId, date, minuteFellAsleep), FallsAsleep(currDate, currMinuteFellAsleep)) => {
      if (date == currDate) {
        if (minuteFellAsleep.nonEmpty) {
          (guardId, currDate, minuteFellAsleep)
        } else {
          (guardId, currDate, Some(currMinuteFellAsleep.toInt))
        }
      } else {
        if (minuteFellAsleep.nonEmpty) {
          updateArray(guardId, minuteFellAsleep.get, 60)
        }
        (guardId, currDate, Some(currMinuteFellAsleep.toInt))
      }
    }
  }

  val (maxGuardId, sleepPattern) = resMap.toList.maxBy(_._2.sum)
  //resMap.foreach { case (k, v) => println(k + s"-> ${v.toList}") }
  println(maxGuardId + "  " + (maxGuardId.toInt * sleepPattern.indexWhere(_ == sleepPattern.max)))
}
