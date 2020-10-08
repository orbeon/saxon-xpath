package java.util.function

@FunctionalInterface
trait Supplier[T] {
  def get: T
}
