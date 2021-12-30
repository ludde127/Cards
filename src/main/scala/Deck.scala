package cards
import scala.collection.mutable.ArrayBuffer

case class Deck(cards: ArrayBuffer[Card]):
  def take(): Card =
    val i = scala.util.Random.nextInt(cards.length)
    val card = cards(i)
    cards.remove(i)
    card

  def takeN(n: Int): Vector[Card] = Vector.fill(n)(take())

def allCards(): Set[Card] =
  var set = Set.empty[Card]
  for x <- 0 until 4 do
    for y <- 0 to Rank.Ace.ordinal do
      set += Card(Suite.fromOrdinal(x), Rank.fromOrdinal(y))
  set

object Deck:
  val numberOfCards = 52

  def random() = Deck(scala.util.Random.shuffle(ArrayBuffer.from(allCards())))



