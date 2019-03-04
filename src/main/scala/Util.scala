import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object Util {

  trait Interpreter[F[_]] {
    def apply[A](program : F[A]) : A
  }



  def run[F[_], A](program: Free[F, A])(interpreter: Interpreter[F]): A = program match {
    case FlatMap(param, continuation) =>  run(continuation(interpreter(param)))(interpreter)
    case Map(param, continuation) => continuation(interpreter(param))
  }
}
