import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random

object Football extends App {
  val teams = Array("Arsenal","Bournemouth","Brighton & Hove Albion","Burnley","Chelsea",
    "Crystal Palace","Everton","Huddersfield Town","Leicester City","Liverpool",
    "Manchester City","Manchester United","Newcastle United","Southampton","Stoke City",
    "Swansea City")

//  val home = Random.shuffle(teams.toBuffer).take(10)
//  val away = teams.filterNot(x => home.contains(x)).toBuffer
//
//  val games = home.zip(away)
//
//  games.foreach {
//    case (x,y) => println(x + " " + Random.nextInt(5) + " " + y + " " + Random.nextInt(5))
//  }
  def round(teams: Array[String]) : ArrayBuffer[String] = {
  val nextRound: ArrayBuffer[String] = new ArrayBuffer
  val awayScores: ArrayBuffer[Int] = new ArrayBuffer
  val n = teams.length
  val home = Random.shuffle(teams.toBuffer).take(teams.length/2)
  val away = teams.filterNot(x => home.contains(x)).toBuffer

  val homeScores = Seq.fill(teams.length/2)(Random.nextInt(5))

  val uniqueScores = Seq.fill(teams.length/2)(Random.nextInt(5))

  homeScores.zip(uniqueScores).foreach {
    case (x, y) if x == y => awayScores.append(5-y)
    case (x, y) if x != y => awayScores.append(y)
  }

  val games = home.zip(homeScores) zip away.zip(awayScores)

  println(s"\nLast $n")

  games.foreach {
    case ((w, x), (y, z)) => println(w + " " + x + " " + y + " " + z)
  }

  games.foreach {
    case ((homeT, x), (awayT, z)) if x > z => nextRound.append(homeT)
    case ((homeT, x), (awayT, z)) if x < z => nextRound.append(awayT)
  }

  if (nextRound.length >= 2) {round(nextRound.toArray)}
  nextRound
}
round(teams)
}
