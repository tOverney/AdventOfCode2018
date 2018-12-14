package ch.overney.aoc2018.day14

object Part1 extends App {
  var resArray = Vector(3,7)
  val afterSize = 765071
  val returnBlockSize = 10
  val returnWhenSize = afterSize + returnBlockSize

  var firstElfPos = 0
  var secondElfPos = 1

  while (resArray.size < returnWhenSize) {
    val elf1Recipe = resArray(firstElfPos)
    val elf2Recipe = resArray(secondElfPos)

    val newValues = elf1Recipe + elf2Recipe
    val toAppend = newValues.toString.toCharArray.map(_.toString.toInt)
    resArray = resArray ++ toAppend
    //println(resArray.mkString(" "))
    val currSize = resArray.size
    firstElfPos = (firstElfPos + 1 + elf1Recipe) % currSize
    secondElfPos = (secondElfPos + 1 + elf2Recipe) % currSize
  }

  println(resArray.slice(afterSize, returnWhenSize).mkString(""))
}
