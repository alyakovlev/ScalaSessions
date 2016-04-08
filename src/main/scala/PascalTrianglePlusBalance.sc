/*
Instructions and Descriptions here:
https://class.coursera.org/progfun-004/assignment/view?assignment_id=4
 */
pascalCheat(0, 0)
pascalRecursive(0, 0)

pascalCheat(7, 7)
pascalRecursive(7, 7)

pascalCheat(13, 6)
pascalRecursive(13, 6)

balance("(if (zer()))((()o? x) max (/ 1 x))".toList)
balance("(-:-)$-)(-%".toList)
balance("()".toList)
balance(")(".toList)

def factorial(n: BigInt): BigInt = {
  if (n <= 1)
    1
  else
    n * factorial(n - 1)
}

//get value from pascal's triangle using standard formula (c from n by k)
//We need to use BigInt here, because if we use just Int - the result after twelfth line will be wrong
def pascalCheat(n: BigInt, k: BigInt): BigInt = {
  if (k > n || k < 0 || n < 0)
    0
  else factorial(n) / (factorial(k) * factorial(n - k))
}

//get value from pascal's triangle using recursion
def pascalRecursive(n: Int, k: Int): Int = {
  if (k > n || k < 0 || n < 0)
    0
  else if (k == 0 || n == k)
    1
  else
    pascalRecursive(n - 1, k - 1) + pascalRecursive(n - 1, k)
}

//"(if (zero? x) max (/ 1 x))" - correct string
//"(-:-)$-)(-%" - incorrect string
def balance(chars: List[Char]): Boolean = {

  def isBalanced(chars: List[Char], numberOfOpenedBraces: Int): Boolean = {
    if (chars.isEmpty) numberOfOpenedBraces == 0
    else if (chars.head == '(')
      isBalanced(chars.tail, numberOfOpenedBraces + 1)
    else if (chars.head == ')')
      numberOfOpenedBraces > 0 && isBalanced(chars.tail, numberOfOpenedBraces - 1)
    else isBalanced(chars.tail, numberOfOpenedBraces)
  }

  isBalanced(chars, 0)

  //alternative way (this works too)
  /*if (chars.isEmpty)
    true
  else {

    val openedBraceIndex = chars.lastIndexOf('(')
    val closedBraceIndex = chars.indexOf(')', openedBraceIndex)

    if (closedBraceIndex >= 0)
      balance(chars.slice(0, openedBraceIndex) ++ chars.slice(closedBraceIndex + 1, chars.size))
    else
      false
  }*/

}