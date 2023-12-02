// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Tests extends munit.FunSuite {
  test("extractFirstLast") {
    val mapping = Map(
      "oneight" -> (1, 8),
      "six2twobgzsfsptlqnine42xtmdprjqc" -> (6, 2),
      "6six7threeh" -> (6,3),
      "fivemxhmgvxfpsxm4" -> (5,4)
    )
    mapping.map((input, output) => assertEquals(extractFirstLast(input), output))
  }

  test("firstLastToInt") {
    val mapping = Map(
      (1, 2) -> 12,
      (5, 9) -> 59
    )
    mapping.map((input, output) => assertEquals(firstLastToInt(input), output))
  }
}
