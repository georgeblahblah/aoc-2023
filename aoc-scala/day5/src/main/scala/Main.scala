import common.file._
import scala.util.matching.Regex.Match
import scala.collection.immutable.NumericRange.Inclusive

@main def main(inputFile: String) = {
  // println(f"Part 1: ${part1(readFile(inputFile))}")
  println(f"Part 2: ${part2(readFile(inputFile))}")
}

def part1(input: String) = {
  val splitInput = input.split("\n")
  val seeds: Iterator[BigInt] =
    for numberMatch <- raw"\d+".r.findAllMatchIn(splitInput(0))
    yield BigInt(numberMatch.matched)

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

def doMapping(lines: Array[String]): Array[(BigInt, BigInt, BigInt)] = {
  val mappingRegex = raw"(\d+) (\d+) (\d+)".r
  for
    line <- lines
    mappings <- mappingRegex
      .findFirstMatchIn(line)
      .map((aMatch) =>
        (
          BigInt(aMatch.group(1)),
          BigInt(aMatch.group(2)),
          BigInt(aMatch.group(3))
        )
      )
    diff = mappings._2 - mappings._1
  // yield (mappings._2.to(mappings._2 + mappings._3) -> diff)
  yield (mappings._2, mappings._2 + mappings._3, diff)
}

def getDiff(
    mapping: Array[(BigInt, BigInt, BigInt)],
    needle: BigInt
): BigInt = mapping
  .find((start, end, _) => start <= needle && end >= needle)
  .getOrElse((0, 0, BigInt(0)))
  ._3

def part2(input: String) = {
  val splitInput = input.split("\n")
  val seeds: Iterator[Inclusive[BigInt]] =
    for
      seedMatch <- raw"(\d+) (\d+)".r.findAllMatchIn(splitInput(0))
      start = BigInt(seedMatch.group(1))
      end = start + BigInt(seedMatch.group(2))
    yield start to end

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

  var smallest: Option[BigInt] = None
  for
    seedRange <- seeds
    seed <- seedRange
    soil = seed - getDiff(seedToSoil, seed)
    fertilizer = soil - getDiff(soilToFertilizer, soil)
    water = fertilizer - getDiff(fertilizerToWater, fertilizer)
    light = water - getDiff(waterToLight, water)
    temperature = light - getDiff(lightToTemperature, light)
    humidity = temperature - getDiff(temperatureToHumidity, temperature)
    location = humidity - getDiff(humidityToLocation, humidity)
  do {
    if (!smallest.isDefined) smallest = Some(location)
    if smallest.isDefined && smallest.get > location then
      smallest = Some(location)
  }
  smallest
}
