import common.file._
@main def main(filePath: String) =
  val lines = readFileLines(filePath).toSeq
  println(f"Part 1 answer: ${part1(lines)}")
  println(f"Part 2 answer: ${part2(lines)}")

def part1(lines: Seq[String]): Int = lines
  .map(Game.fromLine)
  .filter(_.isPossible())
  .map(_.id)
  .sum

def part2(lines: Seq[String]): Int = {
  lines.toSeq
    .map(Game.fromLine)
    .map((game) =>
      GameTurn(
        game.turns.foldLeft(1)((acc, gt) => math.max(acc, gt.red)),
        game.turns.foldLeft(1)((acc, gt) => math.max(acc, gt.green)),
        game.turns.foldLeft(1)((acc, gt) => math.max(acc, gt.blue))
      )
    )
    .map(gt => gt.red * gt.blue * gt.green)
    .sum
}

object GameReader {
  val gameIdExpression = """(?<=Game )\d+(?=:)""".r
  def gameId(line: String): Int =
    gameIdExpression.findFirstMatchIn(line) match {
      case Some(id) => id.matched.toInt
      case None     => 0
    }

  def gameTurns(line: String): Seq[GameTurn] = {
    val turnRegex = """(\d+[ \w,]+)""".r
    val scoreRegex = """(\d+) (\w+)""".r
    turnRegex
      .findAllMatchIn(line)
      .map(_.matched)
      // "1 red, 5 blue, 10 green, 2 red"
      .map((matchString) => {
        val colourScores = scoreRegex
          .findAllMatchIn(matchString)
          // ["1 red", "5 blue", "10 green", "2 red"]
          .map((theMatch) => (theMatch.group(2), theMatch.group(1).toInt))
          // [("red", 1), ("blue", 5), ("green", 10), ("red", 2)]
          .foldLeft(Map[String, Int]())((map, tup) => {
            map + (tup._1 -> (map.getOrElse(tup._1, 0) + tup._2))
          })
        // Map("red" -> 3, "blue" -> 5, "green" -> 10)
        GameTurn(
          colourScores.getOrElse("red", 0),
          colourScores.getOrElse("green", 0),
          colourScores.getOrElse("blue", 0)
        )
      })
      .toSeq
  }
}

case class Game(id: Int, turns: Seq[GameTurn]) {
  val maxRed = 12
  val maxGreen = 13
  val maxBlue = 14

  def isPossible(): Boolean = !this.turns.exists((turn) =>
    turn.red > maxRed || turn.green > maxGreen || turn.blue > maxBlue
  )
}

object Game {
  def fromLine(line: String): Game = Game(
    GameReader.gameId(line),
    GameReader.gameTurns(line)
  )
}

case class GameTurn(red: Int, green: Int, blue: Int)
