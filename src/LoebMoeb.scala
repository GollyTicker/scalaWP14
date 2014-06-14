/**
 * Created by Swaneet on 02.04.14.
 */

object LoebMoeb {


}

trait Functor[F[_]]  { self =>
  /** Lift `f` into `F` and apply to `F[A]`. */
  def fmap[A, B](fa: F[A])(f: A => B): F[B]
}



