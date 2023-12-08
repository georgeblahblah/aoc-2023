import common.file._
import scala.util.matching.Regex.Match

// val input = """467..114..
// ...*......
// ..35..633.
// ......#...
// 617*......
// .....+.58.
// ..592.....
// ......755.
// ...$.*....
// .664.598.."""

@main def main(fileName: String) = {
  val fileContents = readFile(fileName)
  println(f"Part 1: ${part1(fileContents)}")
  println(f"Part 2: ${part2(fileContents)}")
}

def part1(input: String) = {
  val symbolRegex = """([^\d\.\s])""".r
  val splitInput = input.split("\n")

  val symbolMap = (for
    rowCount <- 0 to splitInput.length - 1
    row = splitInput(rowCount)
    symbol <- symbolRegex.findAllMatchIn(row)
  yield ((rowCount, symbol.start) -> symbol.matched)).toMap

  val numberRegex = """\d+""".r
  (for
    rowCount <- 0 to splitInput.length - 1
    row = splitInput(rowCount)
    numberMatch <- numberRegex.findAllMatchIn(row)
  yield
    if (for
        scanRow <- rowCount - 1 to rowCount + 1
        scanCol <- numberMatch.start - 1 to numberMatch.end
        symbol = symbolMap.get((scanRow, scanCol))
        if symbol.isDefined
      yield true).contains(true)
    then numberMatch.matched.toInt
    else 0).sum

}

def part2(input: String) = {}
