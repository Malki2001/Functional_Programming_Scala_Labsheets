object Q1 {
  def main(args: Array[String]): Unit ={
    //Question 01
    val input_1 = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val output_1 = filterEvenNumbers(input_1)
    println("Question 01 :")
    println("[" + output_1.mkString(", ") + "]")

    //Question 02
    val input_2 = List(1, 2, 3, 4, 5)
    val output_2 = calculateSquare(input_2)
    println("Question 02  :")
    println("[" + output_2.mkString(", ") + "]")

    //Question 03
    val input_3 = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val output_3 = filterPrime(input_3)
    println("Question 03 :")
    println("[" + output_3.mkString(", ") + "]")



  }
  //Function 01
  def filterEvenNumbers(numbers: List[Int]): List[Int] = {
    numbers.filter(num => num % 2 == 0) //This lambda function takes a single parameter,num
  }

  //Function 02
  def calculateSquare(numbers: List[Int]): List[Int] = {
    numbers.map(num => num * num)
  }

  //Function 03
  def isPrime(n: Int): Boolean = {
    if (n <= 1) false
    else if (n <= 3) true  // 2 and 3 are prime numbers.
    else if (n % 2 == 0 || n % 3 == 0) false //prime numbers are not divisible by any smaller prime.
    else {
      var i = 5
      while (i * i <= n) {
        if (n % i == 0 || n % (i + 2) == 0)
          return false
        i += 6 //Based on the observation that prime numbers greater than 3 can be written as 6k ± 1
      }
      true
    }
  }


  def filterPrime(numbers: List[Int]): List[Int] = {
    numbers.filter(num => isPrime(num))
  }

}
