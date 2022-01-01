package cards
import scala.util.Random.nextInt

enum Suite:
  case Heart, Diamonds, Spades, Clubs

def randomSuite = Suite.fromOrdinal(nextInt(4))

enum Rank:
  case Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace

  def toInt = ordinal + 2

def rankFromInt(i: Int): Rank = Rank.fromOrdinal(i-2)

def randomRank = Rank.fromOrdinal(nextInt(12))

case class Card(suite: Suite, rank: Rank)

object Card:
  def randomCard(): Card = Card(randomSuite, randomRank)
