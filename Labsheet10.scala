object Labsheet10 {

   def main(args: Array[String]): Unit = {
     //Question 01
     val temperaturesCelsius = List(0.0, 10, 20, 30)
     val averageFahrenheit = calculateAverage(temperaturesCelsius)
     println("Average Fahrenheit temperature: "+averageFahrenheit)

     //Question 02
     val Words = List("apple", "banana", "cherry", "date")
     val totalletterOccurence = countLetterOccurrences(Words)
     println("Total count of letter occurrences:" +totalletterOccurence)
   }

  //Function 01
    def calculateAverage(temperaturesCelsius: List[Double]): Double = {
      val temperaturesFahrenheit = temperaturesCelsius.map(celsius => (celsius * 9 / 5) + 32)
      val totalFahrenheit = temperaturesFahrenheit.reduce((a, b) => a + b)
      val averageFahrenheit = totalFahrenheit / temperaturesFahrenheit.length

      return averageFahrenheit
    }

   //Function 02
    def countLetterOccurrences(Words : List[String]): Int ={
      val lengthCalculate = Words.map(word => (word.length))
      val lettersOccurrence = lengthCalculate.reduce((a,b) => a + b)

      return  lettersOccurrence
    }


  }



