package lms.util

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
