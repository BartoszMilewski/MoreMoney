import scalaz._, Scalaz._

object SendMoreMoney extends App {

  type StateL[A] = StateT[List, List[Int], A]

  def select[A](xs: List[A]): List[(List[A], A)] =
    xs map { x => (xs.filterNot(_ == x), x) }

  def go(chars: List[Char], subst: Map[Char, Int])(i: Int): StateL[(Int, Int, Int)] =
    chars match {
      case c :: Nil => prune(subst + (c -> i))
      case c :: cs  => StateT(select[Int]) >>= go(cs, subst + (c -> i))
    }

  def prune(charToDigit: Map[Char, Int]): StateL[(Int, Int, Int)] = for {
    _ <- (charToDigit('m') == 0).prevent[StateL](())
    _ <- (charToDigit('s') == 0).prevent[StateL](())
    send = "send" map charToDigit reduce (_ * 10 + _)
    more = "more" map charToDigit reduce (_ * 10 + _)
    money = "money" map charToDigit reduce (_ * 10 + _)
    _ <- (send + more == money).guard[StateL](())
  } yield (send, more, money)

  def solutions: StateL[(Int, Int, Int)] =
    StateT(select[Int]) >>= go("sendmoremoney".toList.distinct, Map.empty)

  solutions.eval(0 |-> 9).head |> println
}
