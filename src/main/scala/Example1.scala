import io.circe._
import io.circe.parser._



object Example1 {




  //Little example showing how to parse a simple Json using Circe library

  def main(args: Array[String]): Unit = {


    val jsonString =
      """{
        "name":"Pedro",
        "age":21,
        "numbers": [123456789,987654321]
                         }
       """

    // The parse method returns an instance of the class Either which can be Right if the json was successfully parsed, otherwise it will be an instance of Left

    val jsonResult = parse(jsonString) match {

      case Right(json) => json
      case Left(parsingFailure) => throw new Exception("The jason couldn't be parsed")

    }

    case class Person(name:String,age:Int,numbers:List[Long])


    //We can try to parse the json field by field, just taking the fields of our necessity
    //In order to get field by field we have to take a cursor from the Json object

    val cursor = jsonResult.hcursor


    //Each time we try to get an specific field we will get an Either instance, in case it is a Right instance the value has been extracted correctly

    val name = cursor.downField("name").as[String] match {

      case Right(value) => value
      case Left(decodingFailure) => throw new Exception("There was an error trying to decode the field \"name\"")

    }

    val age = cursor.downField("age").as[Int] match {

      case Right(value) => value
      case Left(decodingFailure) => throw new Exception("There was an error trying to decode the field \"age\"")

    }

    val numbers = cursor.downField("numbers").as[List[Long]] match {

      case Right(value) => value
      case Left(decodingFailure) => throw new Exception("There was an error trying to decode the field \"numbers\"")

    }

    val resultantPerson = Person(name,age,numbers)

    println(resultantPerson)

    // In a more functional approach to the problem we could use the class Option to extract the values so in case a value is imparseable we could still provide a result

    // For example in this Json the field numbers doesn't exist, there will be cases in which we don't know if a field is going to appear in the definiton of the Json or not

    val json2 = """{
        "name":"Pedro",
        "age":21
                   }
       """


    val resultJson = parse(json2) match {
      case Right(success) => success
      case Left(parsingFailure) => throw new Exception("Impossible to parse the Json")


    }

    val cursor2 = resultJson.hcursor


    //We provide another definition for the case class Person

    case class Person2(name:Option[String],age:Option[Int],numbers:Option[List[Long]])


    val optionalName : Option[String] = cursor2.downField("name").as[String] match {
      case Right(success) => Some(success)
      case Left(decodingFailure) => None


    }

    val optionalAge : Option[Int] = cursor2.downField("age").as[Int] match {
      case Right(success) => Some(success)
      case Left(decodingFailure) => None


    }

    val optionalNumbers : Option[List[Long]] = cursor2.downField("numbers").as[List[Long]] match {

      case Right(success) => Some(success)
      case Left(failure) => None

    }

    //Since this time the parsing of the field "numbers" will fail the result of the value "optionalNumbers" will be an instance of None

    val person2 = Person2(optionalName,optionalAge,optionalNumbers)

    println(person2)


  }

}


