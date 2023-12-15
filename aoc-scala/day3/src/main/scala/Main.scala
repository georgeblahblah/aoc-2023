import common.file._
import scala.util.matching.Regex.Match

val input = """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""

@main def main(fileName: String) = {
  val fileContents = readFile(fileName)
  println(f"Part 1: ${part1(fileContents)}")
  println(f"Part 2: ${part2(fileContents)}")
}

def part1(input: String) = {
  val symbolRegex = raw"([^\d\.\s])".r
  val splitInput = input.split("\n")

  val symbolMap = (for
    rowCount <- 0 until splitInput.length
    row = splitInput(rowCount)
    symbol <- symbolRegex.findAllMatchIn(row)
  yield (Position(rowCount, symbol.start) -> symbol.matched)).toMap

  val numberRegex = raw"\d+".r
  (for
    rowCount <- 0 until splitInput.length
    row = splitInput(rowCount)
    numberMatch <- numberRegex.findAllMatchIn(row)
  yield
    if (for
        scanRow <- rowCount - 1 to rowCount + 1
        scanCol <- numberMatch.start - 1 to numberMatch.end
        symbol = symbolMap.get(Position(scanRow, scanCol))
        if symbol.isDefined
      yield true).contains(true)
    then numberMatch.matched.toInt
    else 0).sum
}

def part2(input: String) = {
  val gearRegex = raw"\*".r
  val numberRegex = raw"\d+".r
  val splitInput = input.split("\n")
  (for
    (line, idx) <- splitInput.zipWithIndex
    gearMatch <- gearRegex.findAllMatchIn(line)
    above = if idx > 0 then splitInput(idx - 1) else ""
    below = if idx < splitInput.length - 1 then splitInput(idx + 1) else ""
    numberMatches = numberRegex.findAllMatchIn(above) ++ numberRegex
      .findAllMatchIn(below) ++ numberRegex.findAllMatchIn(line)
    gearNumbers = numberMatches
      .filter((numberMatch) =>
        (numberMatch.start - 1).to(numberMatch.end).contains(gearMatch.start)
      )
      .map(_.matched.toInt)
      .toSeq
    if gearNumbers.length == 2
  yield gearNumbers.foldLeft(1)(_ * _)).sum

}

case class Position(x: Int, y: Int)
