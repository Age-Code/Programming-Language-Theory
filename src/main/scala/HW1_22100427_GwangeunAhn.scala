// The number of problems solved by myself: 6 out of 8

// Problem 1:
// Solved by myself: Y
// Time taken: about 3 mins
// [contract] dollarToWon: Int-> Int
// [purpose] To convert dollar to Won
// [tests] dollarToWon(1) => 1393
//         dollarToWon(2) => 2786
def dollarToWon(dollar: Int): Int = {
  dollar * 1393
}

// Problem 2:
// Solved by myself: Y
// Time taken: about 8 mins
// [contract] max: Int Int-> Int
// [purpose] Find the greater of the two integers
// [tests] max(1, 2) => 2
//         max(4, 3) => 4
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
// [tests] volumeCuboid(3, 3, 3) => 27
//         volumeCuboid(4, 3, 5) => 60
def volumeCuboid(length: Int, breadth: Int, height: Int): Int = {
  length * breadth * height
}

// Problem 4:
// Solved by myself: Y
// Time taken: about 12 mins
// [contract] gcd: Int Int -> Int
// [purpose] To calculate the greatest common divisor
// [tests] gcd(36, 54) => 18
//         gcd(178, 246) => 2
def gcd(a: Int, b: Int): Int = b match {
  case x if x == 0 => a
  case _ => gcd(b, a%b)
}

// Problem 5:
// Solved by myself: Y
// Time taken: about 23 mins
// [contract] factorial: Int -> Int
// [purpose] To calculate the factorial
// [tests] factorial(5) => 120
//         factorial(7) => 5040
def factorial(num: Int): Int = num match {
  case x if x == 0 => 1
  case _ => num * factorial(num - 1)
}
// [contract] combination: Int Int -> Int
// [purpose] To calculate the combination
// [tests] combination(7, 5) => 21
//         combination(5, 2) => 10
def combination(n: Int, k: Int): Int = {
  factorial(n)/(factorial(k)*factorial(n-k))
}

// Problem 6:
// Solved by myself: Y
// Time taken: about 20 mins
trait Vehicle
case class Bicycle(wheels: Int) extends Vehicle
case class Car(wheels: Int, windows: Int) extends Vehicle
case class Airplane(wheels: Int, windows: Int, engines: Int) extends Vehicle

// [contract] vehicleTax: Vehicle -> Int
// [purpose] To calculate the vehicleTax
// [tests] vehicleTax(Bicycle(2)) => 4
//         vehicleTax(Car(4, 8)) => 16
val wheelTax = 2
val windowTax = 1
val engineTax = 3
def vehicleTax(ve: Vehicle): Int = ve match{
  case Bicycle(a) => a*wheelTax
  case Car(a, b) => (a*wheelTax)+(b*windowTax)
  case Airplane(a, b, c) => (a*wheelTax)+(b*windowTax)+(c*engineTax)
}

// [contract] isVehicleSafeBoolean: Vehicle -> Boolean
// [purpose] To calculate the isVehicleSafeBoolean
// [tests] isVehicleSafeBoolean(Bicycle(2)) => true
//         isVehicleSafeBoolean(Car(2, 1)) => false
def isVehicleSafeBoolean(ve: Vehicle): Boolean = ve match{
  case Bicycle(a) => a<4
  case Car(a, b) => a>3 && b>2
  case Airplane(a, b, c) => a>2 && b>10 && c>1

}

// [contract] isVehicleSafe: Vehicle -> String
// [purpose] To calculate the isVehicleSafe
// [tests] isVehicleSafe(Bicycle(2)) => "safe"
//         isVehicleSafe(Car(2, 1)) => "unsafe"
def isVehicleSafe(ve: Vehicle): String = ve match{
  case x if isVehicleSafeBoolean(x) => "safe"
  case _ => "unsafe"
}

// Problem 7:
// Solved by myself: Y
// Time taken: about 34 mins
// [contract] isVehicleSafe: Vehicle -> String
// [purpose] To calculate the isVehicleSafe
// [tests] isVehicleSafe(Bicycle(2)) => "safe"
//         isVehicleSafe(Car(2, 1)) => "unsafe"
def alphabetToName(elem: Char): String = elem match{
  case 'a' => "alice"
  case 'c' => "cherry"
  case 'j' => "jc"
  case 'k' => "kate"
  case _ => "unnamed"
}

// [contract] isVehicleSafe: Vehicle -> String
// [purpose] To calculate the isVehicleSafe
// [tests] isVehicleSafe(Bicycle(2)) => "safe"
//         isVehicleSafe(Car(2, 1)) => "unsafe"
def nameAlphabet(list: List[Char]): List[String] = {
  list.map(elem => alphabetToName(elem))
}

// Problem 8:
// Solved by myself: Y
// Time taken: about 34 mins
// [contract] isVehicleSafe: Vehicle -> String
// [purpose] To calculate the isVehicleSafe
// [tests] isVehicleSafe(Bicycle(2)) => "safe"
//         isVehicleSafe(Car(2, 1)) => "unsafe"
def updateElem(oldName: String, newName: String, elem: String): String = elem match{
  case elem if elem.equals(oldName) => newName
  case _ => elem
}

// [contract] isVehicleSafe: Vehicle -> String
// [purpose] To calculate the isVehicleSafe
// [tests] isVehicleSafe(Bicycle(2)) => "safe"
//         isVehicleSafe(Car(2, 1)) => "unsafe"
def updateName(oldName: String, newName: String, list: List[String]): List[String] = {
  list.map(elem => updateElem(oldName, newName, elem))
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

  // Problem 4 test cases
  assert(gcd(36, 54) == 18, "Test failed for gcd(36, 54)")
  assert(gcd(178, 246) == 2, "Test failed for gcd(178, 246)")

  // Problem 5 test cases
  assert(combination(7, 5) == 21, "Test failed for combination(7, 5)")
  assert(combination(5, 2) == 10, "Test failed for combination(5, 2)")

  println(updateName("cherry","claire",List("jc","cherry","kate")))
}
