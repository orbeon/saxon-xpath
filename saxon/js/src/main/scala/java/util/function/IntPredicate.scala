package java.util.function


@FunctionalInterface
trait IntPredicate {

  def test(value: Int): Boolean

  def and(other: IntPredicate): IntPredicate =
    (value: Int) => test(value) && other.test(value)

  def negate: IntPredicate =
    (value: Int) => !test(value)

  def or(other: IntPredicate): IntPredicate =
    (value: Int) => test(value) || other.test(value)
}
