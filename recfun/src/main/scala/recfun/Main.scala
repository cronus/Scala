package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
     Exercise 1: Pascal’s Triangle
     The following pattern of numbers is called Pascal’s triangle.

     1           1
     2          1 1
     3         1 2 1
     4        1 3 3 1
     5       1 4 6 4 1
     6          ...

     The numbers at the edge of the triangle are all 1, 
     and each number inside the triangle is the sum of the two numbers above it. 
     Write a function that computes the elements of Pascal’s triangle by means of a recursive process.

     Do this exercise by implementing the pascal function in Main.scala, 
     which takes a column c and a row r, counting from 0 and returns the number 
     at that spot in the triangle. For example, pascal(0,2)=1,pascal(1,2)=2 and pascal(1,3)=3.
   */
    def pascal(c: Int, r: Int): Int = {
        if (c < 0) 0
        else if (c > r) 0
        else if (c == 0 && r == 0) 1
        else if (c==1 && r == 0) 1
        else if (c==1 && r == 1) 1
        else pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  
  /**
     Exercise 2
     Write a recursive function which verifies the balancing of parentheses in a string, 
     which we represent as a List[Char] not a String. For example, the function should return true for the following strings:

         (if (zero? x) max (/ 1 x))
         I told him (that it’s not (yet) done). (But he wasn’t listening)

     The function should return false for the following strings:

         :-)
         ())(

     The last example shows that it’s not enough to verify that a string 
     contains the same number of opening and closing parentheses.

     Do this exercise by implementing the balance function in Main.scala. Its signature is as follows:
   */
    def balance(chars: List[Char]): Boolean = {
        def balance_diff(chars: List[Char], needed: Int): Boolean = {
            if (chars.isEmpty) needed == 0
            else 
                if (chars.head == '(') balance_diff(chars.tail, needed + 1)
                else if (chars.head == ')') needed > 0 && balance_diff(chars.tail, needed - 1)
                else balance_diff(chars.tail, needed)
        }
        balance_diff(chars, 0)
    }
  
  /**
     Exercise 3
     Write a recursive function that counts how many different ways 
     you can make change for an amount, given a list of coin denominations. 
     For example, there are 3 ways to give change for 4 if you have coins with denomination 1 and 2: 1+1+1+1, 1+1+2, 2+2.

     Do this exercise by implementing the countChange function inMain.scala. 
     This function takes an amount to change, and a list of unique denominations for the coins. Its signature is as follows:
   */
    def countChange(money: Int, coins: List[Int]): Int = {
        if (money == 0) 1 
        else if (money > 0 && !coins.isEmpty)
            countChange(money - coins.head, coins) + countChange(money, coins.tail)
        else 0
    }
  }
