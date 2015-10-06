package utils

object id {
  def apply(obj: AnyRef) =
    Integer.toHexString(System.identityHashCode(obj))
}
