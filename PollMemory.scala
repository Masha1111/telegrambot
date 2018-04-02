package scala

class PollMemory{

  var poll_list: Map[Int, Poll] = Map()

  def save(poll: Poll): Unit = poll_list += (poll.id -> poll)

  def get(id: Int): Poll = poll_list(id)

  def del(id: Int): Unit = poll_list -= id
}
