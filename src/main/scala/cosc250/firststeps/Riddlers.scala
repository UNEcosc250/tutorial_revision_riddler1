package cosc250.firststeps

import scala.annotation.tailrec



/**
  * This is a puzzle based on a "Riddler" from fivethirtyeight.com
  * 
  * For some square numbers, if we remove the last digit we get another square number.
  * For instance, 256 is 16 * 16, but if we remove the last digit we get 25 (which is 5 * 5)
  * Let's call these numbers "extra perfect squares" 
  * 
  * Write a function that will return all the "extra perfect squares" between 10 and some value max 
  * 
  * Hint: to find if a Double is a whole number, you can check if d % 1 == 0
  * Hint: to knock off the last digit, just divide by 10. Int / Int has type Int 
  *     - that is if you divide an Int by an Int it will throw away any remainder
  * Hint: Math.sqrt(i:Int):Double will give you a number's positive square root
  */ 
def extraPerfectSquares(max:Int):Seq[Int] = 
  def square(i:Int) = Math.sqrt(i) % 1 == 0
  def eps(i:Int) = square(i) && square(i / 10)

  (1 to max).filter(eps)

@main def epsMain = 
  println(extraPerfectSquares(100))

/**
  * A word worth its weight in letters... Another puzzle based on a fivethirtyeight riddler
  * 
  * Let's suppose we assign the letters A to Z the values 1 to 26
  * 
  * Then, let's suppose we add up the values of the letters in words. So, ONE has the value 15 + 14 + 5 = 34
  * and TWO has the value 20 + 23 + 15 = 58.
  * 
  * We'll write longer words without the "AND". So, 1,417 would be ONE THOUSAND FOUR HUNDRED SEVENTEEN, with value
  * 379 and 3,140,275 would be THREE MILLION ONE HUNDRED FORTY THOUSAND TWO HUNDRED SEVENTY FIVE, with value 718.
  * 
  * Write code to find the largest whole number that is less than its value in letters.
  * 
  */
def valueOf(word:String):Int = 
  // Let's put the space at the beginning with value 0
  val letters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  // This might feel familier from Scrabble scoring
  word.map((letter) => letters.indexOf(letter)).sum

    // This one is a bit like roman numerals. We just need to be a bit careful because we say "one hundred" but not "one ninety"
val xillions = Seq(
  1000000 -> "MILLION",
  1000 -> "THOUSAND",
  100 -> "HUNDRED",
  90 -> "NINETY", 80 -> "EIGHTY", 70 -> "SEVENTY", 60 -> "SIXTY", 50 -> "FIFTY", 40 -> "FORTY", 30 -> "THIRTY", 20 -> "TWENTY",
  19 -> "NINETEEN", 18 -> "EIGHTEEN", 17 -> "SEVENTEEN", 16 -> "SIXTEEN", 15 -> "FIFTEEN", 14 -> "FOURTEEN", 13 -> "THIRTEEN", 
  12 -> "TWELVE", 11 -> "ELEVEN", 10 -> "TEN", 9 -> "NINE", 8 -> "EIGHT", 7 -> "SEVEN", 6 -> "SIX", 5 -> "FIVE", 4 -> "FOUR", 
  3 -> "THREE", 2 -> "TWO", 1 -> "ONE" 
)

// We're also going to need to convert numbers to words
def wordify(i:Int):String = 

  // You know what, I think this one's more readable if we do it with some local mutation
  import scala.collection.mutable
  var remainder = i
  val output = new mutable.StringBuilder

  for (num, str) <- xillions if remainder >= num do
    val repeat = remainder / num
    
    if num >= 100 then 
      output.append(wordify(repeat))
      output.append(" ")
      output.append(str)
      output.append(" ")
    else 
      output.append(str)
      output.append(" ")

    remainder -= repeat * num
  end for

  output.toString
      

def notWorthItsWeightInLetters:Int = 
  // It's pretty hard for a number above a thousand to be "worth its weight in letters", so
  // let's just filter that range
  (1 to 1000).filter((x) => x < valueOf(wordify(x))).max

@main def wiwilMain = 
  println(notWorthItsWeightInLetters)


  /**
   * Another Riddler! 
   * 
   * Suppose we have a Tower of Hanoi set.
   * https://en.wikipedia.org/wiki/Tower_of_Hanoi
   * 
   * There are three pegs, from left to right. There are 3 discs, increasing in size, all of which start on the first peg.
   * On any given turn, the player can move the topmost disc from one peg and place it on another peg, so long
   * as it would not put it on a smaller disc.
   * 
   * In this puzzle, we're not going to try for the minimum number of moves. Instead, we're going to say that
   * the the player *always makes a random legal move*. Your job is to measure, over 100 runs, what the average
   * number of moves is before the Tower is solved (before all the discs are in order on the last peg).
   * 
   * For this riddle, I've defined some extension methods for you to complete.
   * 
   * This lets you say things like state.isValid(move) and state.move(move), even though State is just a Seq[Seq[]] 
   * 
   */
type Peg = Seq[Int]
type HanoiState = Seq[Peg]

val start:HanoiState = Seq(Seq(1, 2, 3), Seq(), Seq())

type Move = (Int, Int) // e.g. from peg 1 to peg 2

extension (s:HanoiState)
  /**
   * Produces a new state based on applying the given move
   */
  def applyMove(move:Move):HanoiState = 
    val (from, to) = move

    // We take the head off the "from" peg, and add it to the front of the "to" peg
    s.updated(
      from, s(from).tail
    ).updated(
      to, s(from).head +: s(to)
    )

  /** 
   * A state is valid if no Seq contains a number followed by a smaller number
   * 
   * E.g. Seq(Seq(1, 2, 3), Seq(), Seq()) is valid, but Seq(Seq(1, 3, 2), Seq(), Seq()) is not.
   */
  def isValid:Boolean = 
    def isSorted(seq:Seq[Int]):Boolean = (seq == seq.sorted)
    s.forall(isSorted)

  /** A state is "done" if all three numbers are in the last sequence (i.e. all discs are on the last peg) */
  def isDone:Boolean = s == Seq(Seq.empty, Seq.empty, Seq(1, 2, 3))

  /**
   * All the valid moves from this position
   */
  def validMoves:Seq[Move] =
    // A bit of a mouthful - from every peg to every peg,
    // if the source isn't empty
    // if they're different pegs and either the destination is empty or its top disc is bigger
    for 
      i <- (0 until s.length) if s(i).nonEmpty
      j <- (0 until s.length) if i != j && (s(j).isEmpty || s(j).head > s(i).head)
    yield (i, j)

  /**
   * Chooses a move at random from the list of valid moves
   */
  def randomMove():Move = 
    import scala.util.Random
    val possibilities = s.validMoves
    possibilities(Random.nextInt(possibilities.length))




@main def hanoiMain = 

  // Let's define a runToFinish function that will run the random moves until solved, counting as it goes
  import scala.annotation.tailrec
  @tailrec def runToFinish(i:Int, h:HanoiState):Int = 
    if h.isDone then i else runToFinish(i + 1, h.applyMove(h.randomMove()))

  val runs = for 
    i <- 0 until 100
  yield
    val m = runToFinish(0, start)
    println(s"Run $i took $m moves")
    m
    
  println(s"The average was ${runs.sum / 100.0} moves")