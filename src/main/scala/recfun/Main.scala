package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  // Exercise 1
  // I could also do this without the 1s at the beginning and end of Vectors
  // since that is checked for in pascal
  // but this way the stream completes Pascal's Triangle
  lazy val pStream: Stream[Vector[Int]] = {
    Vector(1) #:: Vector(1,1) #:: pStream.zip(pStream.tail).map { pair => 
    val r2 = pair._2
    var x = r2.length
    var y = Vector(1)
      while (x > 1) { 
        y = Vector(r2(x-2) + r2(x-1)) ++ y
        x-=1 }
      y = Vector(1) ++ y; y }
  }
  
  def pascal(c: Int, r: Int): Int = {
    require(c >= 0, "column must be >= 0")
	require(r >= 0, "row must be >= 0")
	// Can easily set up more rules like below but the problem asks for recursion
	if (c == 0 || c == r) 1 else pStream(r)(c) 
  	}
		  
  // Exercise 2
  def balance(chars: List[Char]): Boolean = {
    if ( chars contains "(".head )
    	if ( (chars indexOf ")".head) > (chars indexOf "(".head) )
    	balance(chars diff "()".toList)
        else false
    else !( chars contains ")".head )
  }

  // Exercise 3
  // Consider removing coins larger than money for potential optimization
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < coins.min || coins isEmpty) 0
    else if (coins contains money) 
    	 1 + countChange(money, coins diff List(money))
    else for {
      coin <- coins
      if coin < money
    } yield countChange(money - coin, coins diff List(coin))
    
    //else find largest coin less than money, and recurse with the remainder
		  // if a coin equals the remainder, then add 1 to result and recurse with same remainder and list-coin
		  // else find largest coin less than money, and recurse with the remainder
		  // etc.
    
  }
}


 test("countChange: unsorted CHF") {
    assert(countChange(300,List(500,5,50,100,20,200,10)) === 1022)
  }
 
 200,100
 200,50
 200,10
 200,5
 