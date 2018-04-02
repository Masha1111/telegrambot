package scala
import scala.util.control.Breaks._

class Read(poll_mem: PollMemory) {
  def parse_line(line: String): Seq[String] = line.trim().split(" ").toSeq

  def parse(all_text: String): Unit = {
    var all = all_text
    //val x = 0
    for(x <- 0 to 1){
      val x = x - 1
      Parser.parse(Parser.parser, all) match {
        case Parser.Success(result, _) => {

          all = all.replace(result(0) + " "+ result(1), "")
          check(result)
        }
        case Parser.Failure(msg, _) => {
          println("ошибка")
          sys.exit()
        }
        case Parser.Error(msg, _) => {
          println("ошибка")
          sys.exit()
        }
      }

    }
  }

  def check(parsed_line: List[String]): Unit = parsed_line(0) match {
    case "/create_poll" => {
      create_poll(parsed_line)
    }
    case "/list" => list
    case "/delete_poll" => delete_poll(parsed_line(1))
    case "/start_poll" => start_poll(parsed_line(1))
    case "/stop_poll" => stop_poll(parsed_line(1))
    case "/result" => result(parsed_line(1))
  }

  def create_poll(parsed_line: Seq[String]): Unit = {
    Parser.parse(Parser.createPoll, parsed_line(1)) match {
      case Parser.Success(result,_) => {
        poll_mem.save(result)
        println(result.id)
    }
      case Parser.Failure(msg, _) => {
        println("ошибка")
        sys.exit()
      }
      case Parser.Error(msg, _) => {
        println("ошибка")
        sys.exit()
      }
    }

  }

  def result(arg: String): Unit ={
    Parser.parse(Parser.result, arg) match {
      case Parser.Success(result,_) => {
        println(poll_mem.get(result).name)
        if (poll_mem.get(result).anon){
          println(" Анонимный")
        }
        else println(" Неанонимный")
      }
      case Parser.Failure(msg, _) => {
        println("ошибка")
        sys.exit()
      }
      case Parser.Error(msg, _) => {
        println("ошибка")
        sys.exit()
      }
    }
  }

  def stop_poll(arg: String): Unit ={
    Parser.parse(Parser.stopPoll, arg) match {
      case Parser.Success(result,_) => {
        try {
          poll_mem.get(result).run match {
            case true => {
              print("success")
              poll_mem.get(result).run = false
            }
            case false => print("он остановлен")
          }
        }
        catch{
          case e: Exception => print("ой, не вышло")
        }
      }
      case Parser.Failure(msg, _) => {
        println("ошибка")
        sys.exit()
      }
      case Parser.Error(msg, _) => {
        print("ошибка")
        sys.exit()
      }
    }
  }

  def delete_poll(arg: String): Unit ={
    Parser.parse(Parser.deletePoll, arg) match {
      case Parser.Success(result,_) => {
        try {
          poll_mem.get(result).run match {
            case true => {
              poll_mem.del(result)
              print("successful")
            }
          }
        }
        catch{
          case e: Exception => print("ой, не вышло")
        }
      }
      case Parser.Failure(msg, _) => {
        println("ошибка")
        sys.exit()
      }
      case Parser.Error(msg, _) => {
        print("ошибка")
        sys.exit()
      }
    }
  }

  def start_poll(arg: String): Unit ={
    Parser.parse(Parser.startPoll, arg) match {
      case Parser.Success(result, _) => {
        try {
          poll_mem.get(result).run match {
            case false => poll_mem.get(result).used match {
              case false => {
                poll_mem.get(result).used = true
                poll_mem.get(result).run = true
                print("success")
              }
              case true => print("айайай")
            }
            case true => print("айайай")
          }
        }
        catch {
          case e: Exception => print("ой, не вышло")
        }
      }
      case Parser.Failure(msg, _) => {
        println("ошибка")
        sys.exit()
      }
      case Parser.Error(msg, _) => {
        print("ошибка")
        sys.exit()
      }
    }
  }

  def list: Unit = {
    val temp_array = poll_mem.poll_list.toList
    for (cop <- temp_array) {
      val a = cop._1
      val b =  cop._2.name
      val c = 10
      print(cop._1, cop._2.name)
    }
  }  }

