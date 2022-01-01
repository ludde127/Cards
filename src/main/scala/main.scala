package cards
import scala.collection.mutable.ArrayBuffer


//@main def main = showTally(1000000)
@main def main = run()

def showTally(n: Int): Unit =
  var map = tally(n)
  println(map)

def tally(n: Int): Map[Hands, Float] =
  var array = ArrayBuffer.empty[Hands]
  for i <- 0 until n do
    if i % (n / 100) == 0 then println(i.toFloat/n.toFloat)
    val deck = Deck.random()
    val hand = Hand.fromDeck(deck, 5)
    array = array.append(hand.classify())
  Map.from(Set.from(array).map(hand => (hand, array.count(_ == hand).toFloat/n.toFloat)))

def run(): Unit =
  val deck = Deck.random()
  val hand = Hand.fromDeck(deck, 5)

  println(hand)