import scala.util.Random

object Football extends App {
  val teams = List("Arsenal", "Bournemouth", "Brighton & Hove Albion", "Burnley", "Chelsea",
    "Crystal Palace", "Everton", "Huddersfield Town", "Leicester City", "Liverpool",
    "Manchester City", "Manchester United", "Newcastle United", "Southampton", "Stoke City",
    "Swansea City")

  val top6 = List("Arsenal", "Chelsea", "Liverpool", "Manchester City", "Manchester United", "Newcastle United")

  def round(teams: List[String]): List[String] = {
    val titleOn = Console.BOLD + Console.RED
    val roundNo = teams.length
    val winner = Random.nextInt(2)

    val homeTeams : List[String] = Random.shuffle(teams.toBuffer).take(teams.length / 2).toList
    val awayTeams : List[String] = teams.filterNot(x => homeTeams.contains(x))

    val homeScores : List[Int] = List.fill(teams.length / 2)(Random.nextInt(6))
    val awayScores : List[Int] = List.fill(teams.length / 2)(Random.nextInt(5))

    // zip home teams to their scores and away teams to their scores
    val games = homeTeams.zip(homeScores) zip awayTeams.zip(awayScores)

    val boostTopSix = games.map {
      case ((homeT, hScore), (awayT, aScore)) if top6.contains(homeT) => ((homeT, Random.nextInt(7)), (awayT, aScore))
      case ((homeT, hScore), (awayT, aScore)) if !top6.contains(homeT) => ((homeT, hScore), (awayT, aScore))
      case ((homeT, hScore), (awayT, aScore)) if top6.contains(awayT) => ((homeT, hScore), (awayT, Random.nextInt(7)))
      case ((homeT, hScore), (awayT, aScore)) if !top6.contains(awayT) => ((homeT, hScore), (awayT, aScore))
    }

    val avoidDraws = boostTopSix.map {
      case ((homeT, hScore), (awayT, aScore)) if hScore == aScore => ((homeT, hScore + 3 - winner), (awayT, aScore + 2 + winner))
      case ((homeT, hScore), (awayT, aScore)) if hScore != aScore => ((homeT, hScore), (awayT, aScore))
    }

    println("\n" + titleOn + s"Last $roundNo" + Console.RESET)

    avoidDraws.foreach {
      case ((homeT, hScore), (awayT, aScore)) => println(homeT + " " + hScore + " " + awayT + " " + aScore)
    }

    val nextRound = avoidDraws.map {
      case ((homeT, hScore), (awayT, aScore)) if hScore >= aScore => homeT
      case ((homeT, hScore), (awayT, aScore)) if hScore < aScore => awayT
    }

    if (nextRound.length >= 2) {
      round(nextRound)
    }
    else {
      println("\n" + titleOn + "Winner" + Console.RESET + "\n" + nextRound(0))
      print("\n" + Console.YELLOW +
        "        ¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶\n" +
        "        ¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶\n" +
        "   ¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶\n" +
        " ¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶\n" +
        "¶¶¶¶      ¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶       ¶¶¶¶\n" +
        "¶¶¶       ¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶        ¶¶¶\n" +
        "¶¶        ¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶        ¶¶¶\n" +
        "¶¶¶     ¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶      ¶¶¶\n" +
        "¶¶¶    ¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶    ¶¶¶¶\n" +
        " ¶¶¶   ¶¶¶ ¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶ ¶¶¶    ¶¶¶\n" +
        " ¶¶¶¶   ¶¶¶ ¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶ ¶¶¶¶  ¶¶¶¶\n" +
        "   ¶¶¶¶  ¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶ ¶¶¶¶¶\n" +
        "    ¶¶¶¶¶¶¶¶ ¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶ ¶¶¶¶¶¶¶¶¶\n" +
        "      ¶¶¶¶¶¶  ¶¶¶¶¶¶¶¶¶¶¶¶¶¶   ¶¶¶¶¶¶\n" +
        "               ¶¶¶¶¶¶¶¶¶¶¶¶\n" +
        "                 ¶¶¶¶¶¶¶¶\n" +
        "                   ¶¶¶¶\n" +
        "                   ¶¶¶¶\n" +
        "                   ¶¶¶¶\n" +
        "                   ¶¶¶¶\n" +
        "               ¶¶¶¶¶¶¶¶¶¶¶¶\n" +
        "            ¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶\n" +
        "            ¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶\n" +
        "            ¶¶¶            ¶¶¶\n" +
        "            ¶¶¶            ¶¶¶\n" +
        "            ¶¶¶            ¶¶¶\n" +
        "            ¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶\n" +
        "            ¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶\n" +
        "          ¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶\n" +
        "         ¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶\n" + Console.RESET)
    }
    nextRound
  }
  round(teams)
}
