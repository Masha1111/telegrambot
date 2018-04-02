package scala
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import scala.util.Try
import scala.util.parsing.combinator._



object Parser extends RegexParsers {
  def parser: Parser[List[String]] = com_parse ~ opt(args) ~ opt(com_parse) ^^ {
    case command ~ Some(line_args) ~ Some(new_command) =>
      List(command, line_args)
    case command ~ Some(line_args) ~ None =>
      List(command, line_args)
    case command ~ None ~ None =>
      List(command, "")
  }

  def args: Parser[String] = " *((.*?) )*(?!/)".r
  def com_parse: Parser[String] = "(/create_poll|/list|/delete_poll|/start_poll|stop_poll|/result)".r
//  def parser(string: String): Unit ={
//  val pattern = new Regex("(/create_poll|/list|/delete_poll|/start_poll|stop_poll|/result).*(\n.*)+")
//  val result = (pattern findFirstIn string)
//  print(result)
//}
// def a: Parser[String] = ".*" ^^ (_ => ')

  private val anonymityMap = Map("yes" -> true, "no" -> false)
  private val visibilityMap = Map("afterstop" -> false, "continuous" -> true)

  def leftBracket: Parser[String] = "((" ^^ (_ => "(")

  def rightBracket: Parser[String] = "))" ^^ (_ => ")")

  def string: Parser[String] = """[_a-zA-Zа-яА-ЯёЁ0-9.,:;'"*&!?]+""".r

  def digits: Parser[Int] = """\d+""".r ^^ (_.toInt)

  def date: Parser[Option[LocalDateTime]] =
    """(\d{2}:){2}\d{2} (\d{2}:){2}\d{2}""".r ^^ {
      date => Try(LocalDateTime.parse(date, DateTimeFormatter.ofPattern("HH:mm:ss yy:MM:dd"))).toOption
    }

  def anonymity: Parser[Boolean] = "(" ~> "yes|no".r <~ ")" ^^ (anonymityMap(_))

  def visibility: Parser[Boolean] = "(" ~> "afterstop|continuous".r <~ ")" ^^ (visibilityMap(_))

  def stringArgument: Parser[String] =
    "(" ~> rep(leftBracket | rightBracket | string) <~ ")" ^^ (xs => xs.mkString(sep=" "))

  def digitArgument: Parser[Int] = "(" ~> digits <~ ")"

  def dateArgument: Parser[Option[LocalDateTime]] = "(" ~> date <~ ")"

  def createPoll: Parser[Poll] =
     stringArgument ~
      opt(
        anonymity ~
          opt(visibility ~
            opt(dateArgument ~
              opt(dateArgument)
            )
          )
      ) ^^ {
      case name ~ None =>
        new Poll(name, true, true, None, None)
      case name ~ Some(anonymity ~ None) =>
        new Poll(name, anonymity, true, None, None)
      case name ~ Some(anonymity ~ Some(visibility ~ None)) =>
        new Poll(name, anonymity, visibility, None, None)
      case name ~ Some(anonymity ~ Some(visibility ~ Some(startTime ~ None))) =>
        new Poll(name, anonymity, visibility, startTime, None)
      case name ~ Some(anonymity ~ Some(visibility ~ Some(startTime ~ Some(endTime)))) =>
        new Poll(name, anonymity, visibility, startTime, endTime)
    }

  def deletePoll: Parser[Int] = digitArgument

  def startPoll: Parser[Int] = digitArgument

  def stopPoll: Parser[Int] = digitArgument

  def result: Parser[Int] = digitArgument
}

