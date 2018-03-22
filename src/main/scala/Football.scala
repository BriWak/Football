import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Football extends App {
  val teams = Array("Arsenal", "Bournemouth", "Brighton & Hove Albion", "Burnley", "Chelsea",
    "Crystal Palace", "Everton", "Huddersfield Town", "Leicester City", "Liverpool",
    "Manchester City", "Manchester United", "Newcastle United", "Southampton", "Stoke City",
    "Swansea City")

  def round(teams: Array[String]): ArrayBuffer[String] = {
    val nextRound: ArrayBuffer[String] = new ArrayBuffer
    val awayScores: ArrayBuffer[Int] = new ArrayBuffer
    val roundNo = teams.length
    val homeTeams = Random.shuffle(teams.toBuffer).take(teams.length / 2)
    val awayTeams = teams.filterNot(x => homeTeams.contains(x)).toBuffer
    val titleOn = Console.BOLD + Console.RED
    val homeScores = Seq.fill(teams.length / 2)(Random.nextInt(5))

    val uniqueScores = Seq.fill(teams.length / 2)(Random.nextInt(5))

    homeScores.zip(uniqueScores).foreach {
      case (x, y) if x == y => awayScores.append(5 - y)
      case (x, y) if x != y => awayScores.append(y)
    }

    val games = homeTeams.zip(homeScores) zip awayTeams.zip(awayScores)

    println("\n" + titleOn + s"Last $roundNo" + Console.RESET)

    games.foreach {
      case ((homeT, hScore), (awayT, aScore)) => println(homeT + " " + hScore + " " + awayT + " " + aScore)
    }

    games.foreach {
      case ((homeT, hScore), (awayT, aScore)) if hScore > aScore => nextRound.append(homeT)
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
