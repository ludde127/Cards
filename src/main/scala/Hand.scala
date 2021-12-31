package cards
import scala.collection.mutable.ArrayBuffer

enum Hands:
  case None,
       Pair,
       TwoPair,
       ThreeOfAKind,
       Straight,
       Flush,
       TwoPlusThree,
       FourOfAKind,
       StraightFlush,
       RoyalStraightFlush

  def toPoints(): Int =
    var points = 0
    if ordinal < 8 then
      points = ordinal
    else points = 52
    points

case class Hand(cards: ArrayBuffer[Card]):
  def high = Rank.fromOrdinal(cards.map(card => card.rank.ordinal).max)
  def low = Rank.fromOrdinal(cards.map(card => card.rank.ordinal).min)


object Hand:
  def fromDeck(deck: Deck, size: Int=5) = Hand(ArrayBuffer.from(deck.takeN(size)))
