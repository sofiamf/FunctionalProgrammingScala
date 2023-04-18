package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()


  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = 
    if c == r then 1
    else if c <= 0 || r <=0 then 1
    else
      pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */

  def balance(chars: List[Char]): Boolean = 
    def aux(chars: List[Char], basket: List[Char]): Boolean = 
      if chars.isEmpty && basket.isEmpty then true
      else if chars.isEmpty && !basket.isEmpty then false
      else {
        chars.head match {
          case '(' => aux(chars.tail,'('::basket)
          case ')' if basket.isEmpty => false
          case ')' if basket.head=='(' => aux(chars.tail,basket.tail)
          case _ => aux(chars.tail,basket) 
        }
      }
    aux(chars,List())
    
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
  
  