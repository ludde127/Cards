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
  var knownDeck = allCards()
  cards.foreach(card => knownDeck -= card)
  def high = Rank.fromOrdinal(cards.map(card => card.rank.ordinal).max).toInt
  def low = Rank.fromOrdinal(cards.map(card => card.rank.ordinal).min).toInt

  // Private as they could be missused
  private def _isColor(): Boolean = Set.from(cards.map(_.suite)).size == 1
  private def _isStraight(): Boolean = awayFromStraight().size == 0

  def meanRank = (cards.map(_.rank.toInt).sum)/(cards.length)
  def medianRank: Int =

    val sorted = Vector.from(Vector.from(Set.from(cards.map(_.rank.toInt))).sorted)
    sorted(_middle(sorted))


  def _middle: Int =
    var middle = -1
    if cards.length % 2 == 0 then
      middle = cards.length/2
    else
      middle = cards.length/2 + 1
    middle

  def _middle(seq: Seq[Int]): Int =
    var middle = -1
    if seq.length % 2 == 0 then
      middle = seq.length/2
    else
      middle = seq.length/2 + 1
    middle

  private def awayFromStraight(): Set[Rank] =
    var errors = Set.empty[Rank]
    var rangeStart = medianRank - (cards.length/2)
    var rangeEnd = medianRank + (cards.length/2)

    if rangeStart < Rank.Two.toInt then rangeStart = Rank.Two.toInt
    if rangeEnd > Rank.Ace.toInt then rangeEnd = Rank.Ace.toInt

    val range = (rangeStart to rangeEnd)
    val ranks = cards.map(_.rank)

    for rank <- range do
      if !ranks.contains(rank) then errors = errors + rankFromInt(rank)
    errors

  def multiples(): Map[Rank, Int] =
    var map = Map[Rank, Int]()
    for rank <- cards.map(_.rank) do
      map += (rank -> cards.count(_.rank == rank))
    //map.foreach((k, v) => println(s"$k:$v"))
    assert(map.values.sum == cards.length)
    map

  def multiplesSuites(): Map[Suite, Int] =
    var map = Map[Suite, Int]()
    for suite <- cards.map(_.suite) do
      map += (suite -> cards.count(_.suite == suite))
    //map.foreach((k, v) => println(s"$k:$v"))
    assert(map.values.sum == cards.length)
    map

  def classify(): Hands =
    var hands = Set[Hands]()
    hands += Hands.None
    //println(s"Color: ${_isColor()} , Straight: ${_isStraight()}")
    if _isColor() then
      if _isStraight() && cards(0).suite == Suite.Heart then hands += Hands.RoyalStraightFlush
      else if _isStraight() then hands += Hands.StraightFlush
      else hands += Hands.Flush
    else if _isStraight() then hands += Hands.Straight
    else
      val multies = multiples()
      val values = Vector.from(multies.values)
      if values.contains(3) && values.contains(2) then
        hands += Hands.TwoPlusThree
      else if values.contains(4) then hands += Hands.FourOfAKind
      else if values.contains(3) then hands += Hands.ThreeOfAKind
      else if values.contains(2) then
        if values.filter(_ == 2).length == 2 then hands += Hands.TwoPair
        else hands += Hands.Pair
    hands.maxBy(_.toPoints())

  override def toString(): String =
    s"Cards:$cards,\nHand:${classify()}\nWorth of Flush:${worthOfColor()}\nWorth of Straight:${worthOfStraight()}"

  def switch(index: Int, deck: Deck): Unit =
    val card = deck.take()
    knownDeck -= card
    cards(index) = card

  def knownDeckFilter(pred: Card => Boolean): Float =
    ((knownDeck.filter(pred).size.toFloat)
      /(knownDeck.size.toFloat))

  def probOf(pred: Card => Boolean, needed: Int): Float =
    var prob = 1.0
    for i <- 1 to needed do
      prob *= ((knownDeck.filter(pred).size.toFloat)
        /(knownDeck.size.toFloat-i+1))
    prob.toFloat

  def leftToColor(): (Suite, Int) =
    var closest = multiplesSuites().maxBy((suite, i) => i)
    (closest(0), cards.length-closest(1))

  def probOfColor(): Float =
    val (suite, needed) = leftToColor()
    probOf(c => c.suite == suite, needed)

  def probOfStraight(): Float =
    val missing = awayFromStraight()
    val needed = missing.size
    probOf(c => missing.contains(c.rank), needed)

  def worthOfColor(): Float = probOfColor() * Hands.Flush.toPoints()
  def worthOfStraight(): Float = probOfStraight() * Hands.Straight.toPoints()


object Hand:
  def fromDeck(deck: Deck, size: Int=5) = Hand(ArrayBuffer.from(deck.takeN(size)))
