package scala
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.text.SimpleDateFormat

class Poll(myname: String, anonimity: Boolean, vis: Boolean, start_data: Option[LocalDateTime], end_data: Option[LocalDateTime]) {
  val name = myname

  val id = myname.hashCode()

  var used = false

  var run = false

  val anon = anonimity

  val vision = vis

  val data_start = start_data

  val data_end = end_data
}
