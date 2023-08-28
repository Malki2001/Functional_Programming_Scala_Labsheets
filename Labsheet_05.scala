import scala.io.StdIn

object Tutorial05 {
  def main(args: Array[String]): Unit = {
    //Question 01
    println("------------------QUESTION 01----------------------");
    print("Enter a number:");
    var input =StdIn.readInt();
    println("Is the input prime?"+prime(input));

    //Question 02
    println("------------------QUESTION 02----------------------");
    println("All prime numbers less than the input :");
    print("\t Input:");
    var num = StdIn.readInt();
    primeSeq(num);



    // Question 03
    println("\n------------------QUESTION 03----------------------");
    print("Enter a number:");
    var N = StdIn.readInt();
    println("Addition of numbers from 1 to N:"+sum(1,N));

    // Question 04
    println("------------------QUESTION 04----------------------");
    print("Enter a number:");
    var M = StdIn.readInt();
    println("Check whether the input number is even:"+isEven(M));

    // Question 05
    println("------------------QUESTION 05----------------------");
    print("Enter a number:");
    var P = StdIn.readInt();
    println("Addition of all even numbers less than given input:"+sumEven(P));

    // Question 06
    println("------------------QUESTION 06----------------------");
    println("First n fibbonacci numbers for given n:");
    print("\t Input:");
    var F = StdIn.readInt();
    FibonaciSeq(F);
  }

  //Function 01
  def GCD(num1:Int, num2:Int):Int= num2 match{
    case num2 if num2==0 => num1
    case num2 if(num2>num1) =>GCD(num2,num1)
    case _ =>GCD(num2, num1%num2)
  }
  def prime(n:Int, i:Int=2):Boolean=n match {
    case n if(n==i) => true;
    case n if(GCD(n,i)>1) => false;
    case _ => prime(n,i+1);

  }

  //Function 02
  def primeSeq(i: Int):Unit={
    if ( i < 2){
      return 0;
    }
    primeSeq(i - 1);
    if( prime(i)){
      print(i + " ");
    }
  }

  //Function 03
  def sum(n:Int ,m:Int):Int={
    if(m >= n){
      n + sum(n+1, m);
    }
    else{
      return  0;
    }
  }

  //Function 04
  def isEven(i : Int):Boolean= i match {
    case 0=> true;
    case _ => isOdd(i-1);
  }

  def isOdd(i: Int):Boolean= {
    !isEven(i);
  }

  //Function 05
  def sumEven(n : Int):Int= n match {
    case n if n <= 0   => return 0;
    case n if n%2 == 0 => n + sumEven(n-2);
    case _ => sumEven(n -1);
  }

  //Function 06
  def Fibonacci(i: Int):Int=i match {
    case 0 => 0;
    case i if (i==1) => 1;
    case _ =>Fibonacci(i-1) + Fibonacci(i-2);
  }

  def FibonaciSeq(i: Int):Unit={
    if(i > 0){
      FibonaciSeq(i - 1);
    }
    print(Fibonacci(i) + " ");

  }




}

