package cards
import scala.collection.mutable.ArrayBuffer

case class Hand(cards: ArrayBuffer[Card]):
  def high = Rank.fromOrdinal(cards.map(card => card.rank.ordinal).max)
  def low = Rank.fromOrdinal(cards.map(card => card.rank.ordinal).min)
  
object Hand:
  def fromDeck(deck: Deck, size: Int=5) = Hand(ArrayBuffer.from(deck.takeN(size)))