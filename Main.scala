object Main{
  def main(args: Array[String]): Unit ={
    val pollMemory = new PollMemory
    var read = new Read(pollMemory)
    val name = "C:\\Users\\lorti\\Documents\\МатМех\\ФИЛП\\telegram\\file.txt"
    val lines = scala.io.Source.fromFile(name).mkString
   // val replaced_lines = lines.replace("(", " ")
    //val second_replaced_lines = replaced_lines.replace(")", " ")
    var replaced_lines = lines.replace("\r\n", " ")

    var s = read.parse(replaced_lines)
//    print(command_with_args)
//    var list = Seq[String]
//    for (line <- lines) {
//      read = new Read(pollMemory)
//      read.check(read.parse_line(line))
//      if line.trim.charAt(0) == '/':
//        {
//        list.
//        }
    }
//  }
}
