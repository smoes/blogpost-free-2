
sealed trait Free[F[_], A]{

  def flatMap[B](f : A => Free[F, B]) : Free[F,B] = this match {
    case Map(param, continuation) =>  (continuation andThen f)(param)
    case FlatMap(param, continuation) => FlatMap(param, continuation andThen (x => x.flatMap(f)))
  }

  def map[B](f: A => B): Free[F,B] = this match {
    case Map(param, continuation) => Map(param, continuation andThen f)
    case FlatMap(_, _) => flatMap(x => Map(x, f))
  }
}


case class Map[F[_], I, A](param : I, continuation: I => A) extends Free[F, A]

case class FlatMap[F[_], I, A](param: F[I], continuation : I => Free[F, A]) extends Free[F,A]


object Free {
  def liftF[F[_], A](command: F[A]): Free[F, A] = FlatMap(command, (a : A) => Map(a, identity[A]))


}