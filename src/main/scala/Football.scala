import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Football extends App {
  val teams = Array("Arsenal", "Bournemouth", "Brighton & Hove Albion", "Burnley", "Chelsea",
    "Crystal Palace", "Everton", "Huddersfield Town", "Leicester City", "Liverpool",
    "Manchester City", "Manchester United", "Newcastle United", "Southampton", "Stoke City",
    "Swansea City")

  val top6 = Array("Arsenal", "Chelsea", "Liverpool", "Manchester City", "Manchester United", "Newcastle United")

  def round(teams: Array[String]): ArrayBuffer[String] = {
    val nextRound: ArrayBuffer[String] = new ArrayBuffer
    val roundNo = teams.length
    val homeTeams = Random.shuffle(teams.toBuffer).take(teams.length / 2)
    val awayTeams = teams.filterNot(x => homeTeams.contains(x)).toBuffer
    val titleOn = Console.BOLD + Console.RED
    val homeScores = Seq.fill(teams.length / 2)(Random.nextInt(6))

    val awayScores = Seq.fill(teams.length / 2)(Random.nextInt(5))

    // zip home scores to the teams and away teams with their scores
    val games = homeTeams.zip(homeScores) zip awayTeams.zip(awayScores)

    val ranked = games.map {
      case ((homeT, hScore), (awayT, aScore)) if top6.contains(homeT) => ((homeT, Random.nextInt(7)), (awayT, aScore))
      case ((homeT, hScore), (awayT, aScore)) if !top6.contains(homeT) => ((homeT, hScore), (awayT, aScore))
      case ((homeT, hScore), (awayT, aScore)) if top6.contains(awayT) => ((homeT, hScore), (awayT, Random.nextInt(7)))
      case ((homeT, hScore), (awayT, aScore)) if !top6.contains(awayT) => ((homeT, hScore), (awayT, aScore))
    }

    val avoidDraws = ranked.map {
      case ((homeT, hScore), (awayT, aScore)) if hScore == aScore => ((homeT, hScore+1), (awayT, aScore))
      case ((homeT, hScore), (awayT, aScore)) if hScore != aScore => ((homeT, hScore), (awayT, aScore))
    }

    println("\n" + titleOn + s"Last $roundNo" + Console.RESET)

    avoidDraws.foreach {
      case ((homeT, hScore), (awayT, aScore)) => println(homeT + " " + hScore + " " + awayT + " " + aScore)
    }

    avoidDraws.foreach {
      case ((homeT, hScore), (awayT, aScore)) if hScore >= aScore => nextRound.append(homeT)
      case ((homeT, hScore), (awayT, aScore)) if hScore < aScore => nextRound.append(awayT)
    }

    if (nextRound.length >= 2) {
      round(nextRound.toArray)
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
