package lms.util

// Many of these functions can be found in Cats, but it's friendlier to not
// depend on it.

object Plumbing {
  extension[T] (seq: Seq[Option[T]])
    def sequence: Option[Seq[T]] =
      seq.foldRight(Option(Nil)) { (opt, acc) =>
        for {
          x <- opt
          xs <- acc
        } yield x :: xs
      }
}
