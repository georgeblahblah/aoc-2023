import common.file._
import scala.util.matching.Regex.Match

val input = """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"""

@main def main(inputFile: String) = {
  println(f"Part 1: ${part1(readFile(inputFile))}")
}

def part1(input: String) = {
  (for line <- input.split("\n")
  gameNumberMatch = raw".*:(.*)\|(.*)".r.findFirstMatchIn(line)
  winningNumberMatches = raw"\d+".r.findAllMatchIn(gameNumberMatch.get.group(1))
  winningNumbers: Set[Int] = winningNumberMatches.foldLeft(Set())(_ + _.matched.toInt)
  playingCardMatches: Seq[Int] = raw"\d+".r.findAllMatchIn(gameNumberMatch.get.group(2)).foldLeft(Seq[Int]())(_ :+ _.matched.toInt)
  winningPlayingCards = playingCardMatches.filter(winningNumbers contains _)
  if winningPlayingCards.length > 0
  yield math.pow(2, winningPlayingCards.length - 1)).sum
}
