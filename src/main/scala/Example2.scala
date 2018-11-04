import io.circe.Decoder
import io.circe.parser._


//In this example we are gonna learn how to parse a Json using the commodity of the case classes

object Example2 {


  def main(args: Array[String]): Unit = {


    //Considering the following Json

    val jsonString = """{
        "name":"Pedro",
        "age":21,
        "numbers": [123456789,987654321]
                         }
       """


    // In order to parse this Json we can create a case class that is analogous to the definition of the Json


    case class Person(name:String,age:Int,number:List[Long])

    // Now that we have a case class that is analogous to the definition of this Json we can create an encoder for it

    val personDecoder :Decoder[Person] = {

      Decoder.forProduct3("name","age","numbers")(Person.apply)

    }

    //Now that we have the respective decoder for the class Person we just apply the method decode of our decoder, this method will return an instance of case class Person containing the values of the Json


    val parsedJson = parse(jsonString) match {

      case Right(success) => success
      case Left(parsingFailure) => throw new Exception("Impossible to parse Json")

    }

    val resultantPerson = personDecoder.decodeJson(parsedJson)

    println(resultantPerson)


    // Now we have decoded a Json using the reflection of our case class
    //Let's suppose again that we are uncertain of some field appearing in the definition of the Json
    //For example the following Json doesn't have a field called "numbers"

    val secondJsonString = """{
        "name":"Pedro",
        "age":21
                   }
       """


    val parsedJson2 = parse(secondJsonString) match {
      case Right(success) => success
      case Left(parsingFailure) => throw new Exception("Impossible to parse Json")

    }

    //We will provide a new case class

    case class Person2(name:Option[String],age:Option[Int],numbers:Option[List[Long]])


    val secondPersonDecoder: Decoder[Person2] = {

      Decoder.forProduct3("name","age","numbers")(Person2.apply)

    }


    val secondPerson = secondPersonDecoder.decodeJson(parsedJson2)


    println(secondPerson)


  }


}
