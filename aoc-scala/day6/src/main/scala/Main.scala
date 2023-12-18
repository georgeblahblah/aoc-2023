import common.file._

@main def main(inputFile: String) = {
  println(f"Part 1: ${part1(readFile(inputFile))}")
  println(f"Part 2: ${part2(readFile(inputFile))}")
}

def part1(input: String) = {
  val splitInput = input.split("\n")
  val times: List[Int] =
    (for time <- raw"\d+".r.findAllMatchIn(splitInput(0))
    yield time.matched.toInt).toList
  val distances =
    (for distance <- raw"\d+".r.findAllMatchIn(splitInput(1))
    yield distance.matched.toInt).toList

  val countWinningOptions = for
    (time, distance) <- times.zip(distances)
    rates = 0 to time
  yield rates.count((rate) => {
    val raceTime = time - rate
    rate * raceTime > distance
  })
  countWinningOptions.product
}

def part2(input: String) = {
  val splitInput = input.split("\n")
  val times =
    (for time <- raw"\d+".r.findAllMatchIn(splitInput(0).filter(_.isDigit))
    yield time.matched.toLong).toList
  val distances =
    (for distance <- raw"\d+".r.findAllMatchIn(splitInput(1).filter(_.isDigit))
    yield distance.matched.toLong).toList

  val countWinningOptions = for
    (time, distance) <- times.zip(distances)
    rates = 0L to time
  yield rates.count((rate) => {
    val raceTime = time - rate
    rate * raceTime > distance
  })
  countWinningOptions.product
}
