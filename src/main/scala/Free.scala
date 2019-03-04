
sealed trait Free[F[_], A]{

  def flatMap[B](f : A => Free[F, B]) : Free[F,B]
  def map[B](f: A => B): Free[F,B]
}


case class Map[F[_], I, A](param : F[I], continuation: I => A) extends Free[F, A] {
  def flatMap[B](f : A => Free[F, B]) : Free[F,B] =
    FlatMap(param, continuation andThen f)
  def map[B](f: A => B): Free[F,B] = Map(param, continuation andThen f)
}

case class FlatMap[F[_], I, A](param: F[I], continuation : I => Free[F, A]) extends Free[F,A] {
  def flatMap[B](f: A => Free[F, B]) = FlatMap(param, continuation andThen (x => x.flatMap(f)))
  def map[B](f: A => B) = FlatMap(param, continuation andThen (x => x.map(f)))


}


object Free {
  def liftF[F[_], A](command: F[A]): Free[F, A] = Map(command, identity[A](_))


}