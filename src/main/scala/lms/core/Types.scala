package lms.core

object Types {
  case class Id(val i: Int) {
    def equals(other: Id): Boolean = i == other.i
  }
}
