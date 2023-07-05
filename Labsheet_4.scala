
import scala.io.StdIn

object Labsheet_4 {
  def main(args: Array[String]): Unit = {
    //Question 01
    println("---------------Question 01-----------------");
    println("Enter the deposited money:");
    val depositmoney = StdIn.readDouble();

    val Interest = interest(depositmoney)
    println("Amount of interest:" + Interest);

    //Question 02
    println("\n---------------Question 02-----------------");
    println("Enter an integer:");
    val input = StdIn.readInt();

    println("Output :"+ PatternMatching(args(0).toInt)
    //println("Output :" + PatternMatching(input));

    //Question 03
    println("\n---------------Question 03-----------------");
    var names_array: List[String] = List("Benny", "Niroshan", "Saman", "Kumara");
    var function_array: List[String => String] = List(toUpper, mixString, toLower, BegintoEnd);

    //println(formatNames(names(0),toUpper()));
    // println(formatNames(names(1),mixString()));
    // println(formatNames(names(3),toLower()));
    // println(formatNames(names(4),BegintoEnd()));
    var i = 0;
    while (i < (function_array.length)) {

      println(formatNames(names_array(i), function_array(i)));
      i += 1

    }
  }


    //Function 01
    def interest(deposit: Double): Double = deposit match {
      case deposit if deposit < 0 => 0
      case deposit if deposit < 20000 => deposit * 2.0 / 100
      case deposit if deposit < 200000 => deposit * 4.0 / 100
      case deposit if deposit < 2000000 => deposit * 3.5 / 100
      case deposit if deposit >= 2000000 => deposit * 6.5 / 100

    }

    //Function 02
    def PatternMatching(num: Int): String = num match {
      case num if num <= 0 => "Negative/Zero is input"
      case num if (num % 2 == 0) => "Even number is given "
      case _ => "Odd number is given"
    }

    //Function 03
    def toUpper(input: String): String = {
      input.toUpperCase();
    }

    def toLower(input: String): String = {
      input.toLowerCase();
    }

    def mixString(input: String): String = {
      var len = input.length();
      if (len > 2) {
       (input.substring(0, 2).toUpperCase() + input.substring(2, len).toLowerCase());
      }
      else {
        input.toUpperCase();
      }
    }

    def BegintoEnd(input: String): String = {
      var len = input.length();
      if (len > 2) {
        (input.substring(0, 1).toUpperCase() + input.substring(2, len - 1).toLowerCase()
          + input.substring(len - 1, len).toUpperCase());
      }
      else {
         input.toUpperCase();
      }
    }

    //accepts a name as input and a function (String => String) as a parameter group.
    // This function takes a String input and returns a formatted String output.
    // Inside the formatNames method, the given format function is applied to the input name.
    def formatNames(name: String,formatFn: String => String): String = {
      formatFn(name);
    }


}

