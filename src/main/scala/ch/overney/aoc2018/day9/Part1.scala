package ch.overney.aoc2018.day9

object Part1 extends App {
  val InfoExtractor = """(\d+) players; last marble is worth (\d+) points""".r

  val InfoExtractor(players, lastMarble) = Input.rawData
  val pCount = players.toInt
  val lMarbleId = lastMarble.toInt

  val (_, finalCircle, _, finalScores) = (1 to lMarbleId).foldLeft((1, List(0), 0, Map[Int, Int]())) {
    case ((currPlayer, circle, oldInsertSpot, scores), marbleId) =>
      def nextPId = (currPlayer % pCount) + 1

      if (marbleId % 23 == 0) {
        val previousPScore = scores.getOrElse(currPlayer, 0)
        var idToRemove = oldInsertSpot - 7
        while (idToRemove < 0) {
          idToRemove = idToRemove + circle.size
        }
        val otherMarble = circle(idToRemove)

        val newScores = scores.updated(currPlayer, previousPScore + marbleId + otherMarble)
        (nextPId, circle.filterNot(_ == otherMarble), idToRemove, newScores)
      } else {
        val newInsertSpot = (oldInsertSpot + 1) % circle.size + 1

        val (before, after) = circle.splitAt(newInsertSpot)
        val newCircle = (before :+ marbleId) ++ after

        (nextPId, newCircle, newInsertSpot, scores)
      }
  }
  println(finalScores.maxBy(_._2))
}
