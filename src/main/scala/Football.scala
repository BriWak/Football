import scala.util.Random

case class Team(name: String)

object Football extends App {
  val teams = List(Team("Arsenal"), Team("Bournemouth"), Team("Brighton & Hove Albion"), Team("Burnley"), Team("Chelsea"),
    Team("Crystal Palace"), Team("Everton"), Team("Huddersfield Town"), Team("Leicester City"), Team("Liverpool"),
      Team("Manchester City"), Team("Manchester United"), Team("Newcastle United"), Team("Southampton"), Team("Stoke City"),
        Team("Swansea City"))

  val top6 = List(Team("Arsenal"), Team("Chelsea"), Team("Liverpool"), Team("Manchester City"), Team("Manchester United"), Team("Newcastle United"))

  def round(teams: List[Team]): List[Team] = {
    val titleOn = Console.BOLD + Console.RED
    val roundNo = teams.length
    val winner = Random.nextInt(2)

    val homeTeams : List[Team] = Random.shuffle(teams.toBuffer).take(teams.length / 2).toList
    val awayTeams : List[Team] = teams.filterNot(x => homeTeams.contains(x))

    val homeScores : List[Int] = List.fill(teams.length / 2)(Random.nextInt(6))
    val awayScores : List[Int] = List.fill(teams.length / 2)(Random.nextInt(5))

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
      case ((homeT, hScore), (awayT, aScore)) => println(homeT.name + " " + hScore + " " + awayT.name + " " + aScore)
    }

    val nextRound = avoidDraws.map {
      case ((homeT, hScore), (awayT, aScore)) if hScore >= aScore => homeT
      case ((homeT, hScore), (awayT, aScore)) if hScore < aScore => awayT
    }

    if (nextRound.length >= 2) {
      round(nextRound)
    }
    else {
      println("\n" + titleOn + "Winner" + Console.RESET + "\n" + nextRound.head.name)
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
