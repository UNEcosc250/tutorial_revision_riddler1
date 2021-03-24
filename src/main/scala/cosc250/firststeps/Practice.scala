package cosc250.firststeps


/** 
 * Rotate a list N places to the left. For instance 
 * rotate(3, List(1, 2, 3, 4, 5, 6)) would be List(4, 5, 6, 1, 2, 3)
 */
def rotate[T](by:Int, list:List[T]):List[T] =
  // Let's cheekily use two methods you might not have seen! 
  list.drop(by) ++ list.take(by)

/**
 * Pack consecutive duplicates in a list into sublists
 * 
 * For instance pack(List(1, 1, 1, 2, 2, 3, 4, 4))
 * would be List(List(1, 1, 1), List(2, 2), List(3), List(4, 4))
 * 
 */
def pack[T](list:List[T]):List[List[T]] = 
  // Let's do this one with a fold
  val r = list.foldLeft(List.empty[List[T]])( (result, el) => {
    // The element we're looking at is in l
    // The list we're composing is in result
    // If the head of the result is a list with this element, add this element to that list
    // Otherwise, start a new list and stick it on the front
    result match 
      case (h :: tail) :: outerTail if h == el => (el :: h :: tail) :: outerTail
      case _ => List(el) :: result
  })

  // Unfortunately, it comes out backwards
  r.reverse

  

/**
 * Sort a list of lists according to the length of the sublist
 * 
 * For instance, sortByLength(List(List(1, 1, 1), List(2, 2), List(4)))
 * would be List(List(4), List(2, 2), List(1, 1, 1))
 */
def sortByLength[T](outer:List[List[T]]):List[List[T]] = 
  // Realistically, what we need is a sort function that will let us give it something to sort by
  // Fortunately for us, there already is one, so we can actually just say
  outer.sortBy((l) => l.length)


/**
 * FoldLeft on a tree
 * 
 * Given the tree structure below, implement foldLeft for the tree.
 * Hint: For Empty, you've only got the start value to return.
 *       For Leaf, you want to call f. The two arguments you've got of the right type
 *       are start and value.
 *       For Branch, call foldLeft on the left subtree, get the result, 
 *       and pass that into foldLeft on the right subtree.  
 */ 
enum Tree[+T]:
  def foldLeft[B](start:B)(f: (B, T) => B):B = this match
    case Empty => start
    case Branch(left, right) => 
      val lresult = left.foldLeft(start)(f)
      right.foldLeft(lresult)(f)
    case Leaf(value) => f(start, value)

  case Empty
  case Branch(left:Tree[T], right:Tree[T])
  case Leaf(value:T)


extension (t:Tree[Int])
  def sum:Int = t.foldLeft(0)(_ + _)

  def printFold() = 
    t.foldLeft(0)((i, x) => {
      println(s"$i. Value in this node is $x")
      i + 1
    })

/** Just a main method for you to use as you write your code */
@main def practiceMain = 
  import Tree._
  val tree = Branch(Branch(Leaf(1), Empty), Branch(Leaf(2), Leaf(3)))
  println(tree.sum)
