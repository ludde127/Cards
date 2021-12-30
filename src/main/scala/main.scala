package cards
@main def main = run()

def run(): Unit =
  val deck = Deck.random()
  println(Hand.fromDeck(deck, 5))