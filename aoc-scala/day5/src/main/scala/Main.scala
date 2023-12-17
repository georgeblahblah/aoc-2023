import common.file._
import scala.util.matching.Regex.Match
import scala.collection.immutable.NumericRange.Inclusive

@main def main(inputFile: String) = {
  println(f"Part 1: ${part1(readFile(inputFile))}")
  println(f"Part 2: ${part2(readFile(inputFile))}")
}

def part1(input: String) = {
  val splitInput = input.split("\n")
  val seeds: Iterator[BigInt] =
    for numberMatch <- raw"\d+".r.findAllMatchIn(splitInput(0))
    yield BigInt(numberMatch.matched)

  val mappingRegex = raw"(\d+) (\d+) (\d+)".r
  val seedToSoil = doMapping(
    splitInput
      .drop(splitInput.indexOf("seed-to-soil map:") + 1)
      .takeWhile(_ != "")
  )

  val soilToFertilizer = doMapping(
    splitInput
      .drop(splitInput.indexOf("soil-to-fertilizer map:") + 1)
      .takeWhile(_ != "")
  )

  val fertilizerToWater = doMapping(
    splitInput
      .drop(splitInput.indexOf("fertilizer-to-water map:") + 1)
      .takeWhile(_ != "")
  )

  val waterToLight = doMapping(
    splitInput
      .drop(splitInput.indexOf("water-to-light map:") + 1)
      .takeWhile(_ != "")
  )

  val lightToTemperature = doMapping(
    splitInput
      .drop(splitInput.indexOf("light-to-temperature map:") + 1)
      .takeWhile(_ != "")
  )

  val temperatureToHumidity = doMapping(
    splitInput
      .drop(splitInput.indexOf("temperature-to-humidity map:") + 1)
      .takeWhile(_ != "")
  )

  val humidityToLocation = doMapping(
    splitInput
      .drop(splitInput.indexOf("humidity-to-location map:") + 1)
      .takeWhile(_ != "")
  )

  val locations = for
    seed <- seeds
    soil = seed - getDiff(seedToSoil, seed)
    fertilizer = soil - getDiff(soilToFertilizer, soil)
    water = fertilizer - getDiff(fertilizerToWater, fertilizer)
    light = water - getDiff(waterToLight, water)
    temperature = light - getDiff(lightToTemperature, light)
    humidity = temperature - getDiff(temperatureToHumidity, temperature)
    location = humidity - getDiff(humidityToLocation, humidity)
  yield location
  locations.min
}

def doMapping(lines: Array[String]): Array[(Inclusive[BigInt], BigInt)] = {
  val mappingRegex = raw"(\d+) (\d+) (\d+)".r
  for
    line <- lines
    mappings <- mappingRegex
      .findFirstMatchIn(line)
      .map((aMatch) =>
        (
          BigInt(aMatch.group(1)),
          BigInt(aMatch.group(2)),
          BigInt(aMatch.group(3).toInt)
        )
      )
    diff = mappings._2 - mappings._1
  yield (mappings._2.to(mappings._2 + mappings._3) -> diff)
}

def getDiff(
    mapping: Array[(Inclusive[BigInt], BigInt)],
    needle: BigInt
): BigInt = mapping
  .find((range, _) => range contains needle)
  .getOrElse((needle to needle, BigInt(0)))
  ._2

def part2(input: String) = {
  ""
}
