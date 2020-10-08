package java.util.function


@FunctionalInterface
trait BiConsumer[T, U] {

  def accept(t: T, u: U): Unit

  def andThen(after: BiConsumer[_ >: T, _ >: U]): BiConsumer[T, U] = {
    require(after ne null)
    (l: T, r: U) => {
      accept(l, r)
      after.accept(l, r)
    }
  }
}
