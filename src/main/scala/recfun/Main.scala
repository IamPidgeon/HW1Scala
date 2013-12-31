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
    Vector(1) #:: Vector(1,1) #:: 
      pStream.zip(pStream.tail).map { pair => 
        nextVector(pair._2) 
    	}
  }
  
  def nextVector(lastVec: Vector[Int]) = {
      val vec1 = Vector(1)
      def buildVector(lastLen: Int): Vector[Int] = {
        if (lastLen == lastVec.length)
          buildVector(lastLen-1) ++ Vector(lastVec(lastLen-2) + lastVec(lastLen-1)) ++ vec1
        else if (lastLen > 1)
          buildVector(lastLen-1) ++ Vector(lastVec(lastLen-2) + lastVec(lastLen-1))
        else vec1
      }
      buildVector(lastVec.length)   
  }
  
  def pascal(c: Int, r: Int): Int = {
    require(c >= 0, "column must be >= 0")
	require(r >= 0, "row must be >= 0")
	// Can set up more rules like below but the problem asks for recursion
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
  def countChange(money: Int, coins: List[Int]) = {
    def tailRec(money: Int, coins: List[Int], sum: Int): Int = {
      if (coins.isEmpty || money < coins.min) sum
      else if (coins contains money) 
   	    tailRec(money, coins diff List(money), sum + 1)
   	  // Sum up the coin combinations using tail recursive calls
      else coins.filter(money > _).foldLeft (sum) ((foldedSum, coin) => 	
      	     tailRec(money - coin, coins diff (coins.filter (coin < _)), foldedSum)) 
    }
    tailRec(money, coins, 0)
  }
}
 