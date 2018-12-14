package ch.overney.aoc2018.day14

object Part2 extends App {
  var resArray = Vector(3,7)
  val patternToFind = Input.Data(4)
  val afterSize = 765071
  val returnBlockSize = 10
  val returnWhenSize = afterSize + returnBlockSize

  var firstElfPos = 0
  var secondElfPos = 1
  var result = -1

  while (result == -1) {
    val elf1Recipe = resArray(firstElfPos)
    val elf2Recipe = resArray(secondElfPos)

    val newValues = elf1Recipe + elf2Recipe
    val toAppend = newValues.toString.toCharArray.map(_.toString.toInt)
    resArray = resArray ++ toAppend
    //println(resArray.mkString(" "))
    val currSize = resArray.size
    firstElfPos = (firstElfPos + 1 + elf1Recipe) % currSize
    secondElfPos = (secondElfPos + 1 + elf2Recipe) % currSize
    val patternIdx = resArray.takeRight(1 + returnBlockSize).indexOfSlice(patternToFind)
    if (patternIdx != -1) {
      result = patternIdx + resArray.size - (1 + returnBlockSize)
    }
  }

  println(result)
}
