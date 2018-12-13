package ch.overney.aoc2018.day01

import scala.util.Try

object Part2 extends App {
  val dataStream: Stream[Long] = Input.Data.toStream #::: dataStream
  val result = Try {
    dataStream.foldLeft((0L, Set[Long](0))) {
      case ((acc, seen), curr) =>
        val newAcc = acc + curr
        if (seen(newAcc)) sys.error(s"$newAcc seen twice!")
        (newAcc, seen + newAcc)
    }
  }.failed.map(_.getMessage).getOrElse("Error")
  println(result)
}
