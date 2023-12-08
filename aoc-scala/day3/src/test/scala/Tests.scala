class Tests extends munit.FunSuite {
  test("readGame") {
    val mapping = Map(
      "Game 1: 1 red, 5 blue, 10 green; 5 green, 6 blue, 12 red; 4 red, 10 blue, 4 green" -> Game(
        id = 1,
        Seq(GameTurn(1, 10, 5), GameTurn(12, 5, 6), GameTurn(4, 4, 10))
      ),
      "Game 2: 2 green, 1 blue; 1 red, 2 green; 3 red, 1 blue; 2 blue, 1 green, 8 red; 1 green, 10 red; 10 red" -> Game(
        id = 2,
        Seq(
          GameTurn(0, 2, 1),
          GameTurn(1, 2, 0),
          GameTurn(3, 0, 1),
          GameTurn(8, 1, 2),
          GameTurn(10, 1, 0),
          GameTurn(10, 0, 0)
        )
      )
    )
    mapping.map((input, output) => assertEquals(Game.fromLine(input), output))
  }

  test("isGamePossible") {
    Map(
      Game(1, Seq(GameTurn(10, 10, 10))) -> true,
      Game(2, Seq(GameTurn(10, 0, 0), GameTurn(13, 1, 1))) -> false,
      Game(3, Seq(GameTurn(1, 14, 1))) -> false,
      Game(4, Seq(GameTurn(1, 1, 15))) -> false
    ).map((input, output) => assertEquals(input.isPossible(), output))
  }
}
