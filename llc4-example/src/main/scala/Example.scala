import llc4.js._

object Example extends App {
  js {
    def sum(from: Int, to: Int) = {
      var i = from
      var a = 0
      while (i < to) {
        a += i
        i += 1
      }
      a
    }

    val a = sum(1, 10)
  }
}
