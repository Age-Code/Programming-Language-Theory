// The number of problems solved by myself: 6 out of 8

// Problem 1:
// Solved by myself: Y
// Time taken: about 3 mins
// [contract] dollarToWon: Int-> Int
// [purpose] To convert dollar to Won
// [tests] dollarToWon(1) => 1393)
//         dollarToWon(2) => 2786)
def dollarToWon(dollar: Int): Int = {
  dollar * 1393
}

// Problem 2:
// Solved by myself: Y
// Time taken: about 8 mins
// [contract] max: Int Int-> Int
// [purpose] Find the greater of the two integers
// [tests] max(1, 2) => 2)
//         max(4, 3) => 4)
def max(a: Int, b:Int): Int = a match {
  case x if x > b => x
  case _ => b
}
// [contract] mileToKm: Int Int Int-> Int
// [purpose] Find the greater of the three integers
// [tests] maxOfThreeIntegers(1, 2, 3) => 3
//         maxOfThreeIntegers(5, 6, 4) => 6
def maxOfThreeIntegers(a: Int, b: Int, c: Int): Int = {
  max(max(a,b),max(b,c))
}

// Problem 3:
// Solved by myself: Y
// Time taken: about 6 mins
// [contract] volumeCuboid: Int Int Int-> Int
// [purpose] To calculate the volum of a cuboid
// [tests] volumeCuboid(3, 3, 3) => 27)
//         volumeCuboid(4, 3, 5) => 60)
def volumeCuboid(length: Int, breadth: Int, height: Int): Int = {
  length * breadth * height
}



@main def run(): Unit = {
  // Problem 1 test cases
  assert(dollarToWon(1) == 1393, "Test failed for dollarToWon(1)")
  assert(dollarToWon(2) == 2786, "Test failed for dollarToWon(2)")

  // Problem 2 test cases
  assert(maxOfThreeIntegers(1, 2, 3) == 3, "Test failed for maxOfThreeIntegers(1, 2, 3)")
  assert(maxOfThreeIntegers(5, 6, 4) == 6, "Test failed for maxOfThreeIntegers(5, 6, 4)")

  // Problem 3 test cases
  assert(volumeCuboid(3, 3, 3) == 27, "Test failed for volumeCuboid(3, 3, 3)")
  assert(volumeCuboid(4, 3, 5) == 60, "Test failed for volumeCuboid(4, 3, 5)")
}
