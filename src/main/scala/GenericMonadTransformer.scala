import scalaz.{Functor, Monad, Traverse}
import scalaz._
import Scalaz._


object GenericMonadTransformer {

  implicit class MonadicComposerOps[G[_]](gm: Monad[G]) {

    def composeWithMonad[F[_]](implicit fm: Monad[F], ft: Traverse[F]) =
      new Monad[({type l[A] = G[F[A]]})#l] {
        implicit val gMonad: Monad[G] = gm

        override def bind[A, B](gfa: G[F[A]])(f: (A) => G[F[B]]): G[F[B]] = {
          val gfgfb: G[F[G[F[B]]]] = gfa.map(a => fm.map(a)(f))
          val ggffb: G[G[F[F[B]]]] = gfgfb.map(fgfb => ft.traverse(fgfb)(identity))
          val gfb: G[F[B]] = ggffb.flatMap(identity).map(_.flatMap(identity))

          gfb
        }

        override def point[A](a: => A): G[F[A]] = {
          val fa = fm.point(a)
          val gfa = gm.point(fa)
          gfa
        }
      }
  }


  implicit class WrapperOps4[A[_], B[_], C[_], D[_], Z](m: A[B[C[D[Z]]]]) {
    def composeMonads4 = new MonadWrapper4[A, B, C, D, Z](m)
  }

  implicit class WrapperOps3[A[_], B[_], C[_], Z](m: A[B[C[Z]]]) {
    def composeMonads3 = new MonadWrapper3[A, B, C, Z](m)
  }

  implicit class WrapperOps2[A[_], B[_], Z](m: A[B[Z]]) {
    def composeMonads2 = new MonadWrapper2[A, B, Z](m)
  }

  implicit class WrapperOps1[A[_], Z](m: A[Z]) {
    def composeMonads1 = new MonadWrapper1[A, Z](m)
  }


  class MonadWrapper4[A[_], B[_], C[_], D[_], Z](val run: A[B[C[D[Z]]]]) {

    def apply[W](f: Z => W)(implicit fa: Functor[A], fb: Functor[B], fc: Functor[C], fd: Functor[D]): A[B[C[D[W]]]] = map(f).run

    def up = new MonadWrapper3[A, B, C, D[Z]](run)

    def map[W](f: Z => W)(implicit fa: Functor[A], fb: Functor[B], fc: Functor[C], fd: Functor[D]): MonadWrapper4[A, B, C, D, W] =
      new MonadWrapper4(fa.compose[B].compose[C].compose[D].map(run)(f))

    def map2[W](f: Z => D[W])(implicit fa: Functor[A], fb: Functor[B], fc: Functor[C], md: Monad[D]): MonadWrapper4[A, B, C, D, W] = {
      val fabc = fa.compose[B].compose[C]
      val run1 = fabc.map(run)(dz => md.bind(dz)(f))
      new MonadWrapper4(run1)
    }

    def map3[W](f: Z => C[D[W]])(implicit fa: Functor[A], fb: Functor[B], mc: Monad[C], md: Monad[D], td: Traverse[D]): MonadWrapper4[A, B, C, D, W] = {
      val mcd = mc.composeWithMonad[D]
      val run1 = fa.compose[B].map(run)(cdz => mcd.bind(cdz)(f))
      new MonadWrapper4(run1)
    }

    def map4[W](f: Z => B[C[D[W]]])(implicit fa: Functor[A], mb: Monad[B], mc: Monad[C], md: Monad[D], tc: Traverse[C], td: Traverse[D]): MonadWrapper4[A, B, C, D, W] = {
      type BC[T] = B[C[T]]
      val mbc: Monad[BC] = mb.composeWithMonad[C]
      val mbcd = mbc.composeWithMonad[D]
      val run1 = fa.map(run)(bcdz => mbcd.bind(bcdz)(f))
      new MonadWrapper4(run1)
    }

    def map5[W](f: Z => A[B[C[D[W]]]])(implicit ma: Monad[A], mb: Monad[B], mc: Monad[C], md: Monad[D], td: Traverse[D], tc: Traverse[C], tb: Traverse[B]): MonadWrapper4[A, B, C, D, W] = {
      type AB[T] = A[B[T]]
      type ABC[T] = AB[C[T]]
      val mab: Monad[AB] = ma.composeWithMonad[B]
      val mabc: Monad[ABC] = mab.composeWithMonad[C]
      val mabcd = new MonadicComposerOps(mabc).composeWithMonad[D]
      val run1 = mabcd.bind(run)(f)
      new MonadWrapper4(run1)
    }

    def flatMap[W](f: Z => MonadWrapper4[A, B, C, D, W])(implicit ma: Monad[A], mb: Monad[B], mc: Monad[C], md: Monad[D], td: Traverse[D], tc: Traverse[C], tb: Traverse[B]): MonadWrapper4[A, B, C, D, W] =
      map5(f.andThen(_.run))
  }

  class MonadWrapper3[A[_], B[_], C[_], Z](val run: A[B[C[Z]]]) {

    def down[D[_], W](implicit fa: Functor[A], fb: Functor[B], fc: Functor[C], ev: Z =:= D[W]): MonadWrapper4[A, B, C, D, W] = {
      val res = fa.compose[B].compose[C].map(run)(ev)
      new MonadWrapper4[A, B, C, D, W](res)
    }

    def up = new MonadWrapper2[A, B, C[Z]](run)

    def apply[W](f: Z => W)(implicit fa: Functor[A], fb: Functor[B], fc: Functor[C]): A[B[C[W]]] = map(f).run

    def map[W](f: Z => W)(implicit fa: Functor[A], fb: Functor[B], fc: Functor[C]): MonadWrapper3[A, B, C, W] = {
      val run1 = fa.compose[B].compose[C].map(run)(f)
      new MonadWrapper3[A, B, C, W](run1)
    }

    def map2[W](f: Z => C[W])(implicit fa: Functor[A], fb: Functor[B], fc: Monad[C]): MonadWrapper3[A, B, C, W] = {
      val fab = fa.compose[B]
      val run1 = fab.map(run)(cz => fc.bind(cz)(z => f(z)))
      new MonadWrapper3[A, B, C, W](run1)
    }

    def map3[W](f: Z => B[C[W]])(implicit fa: Functor[A], fb: Monad[B], fc: Monad[C], tc: Traverse[C]): MonadWrapper3[A, B, C, W] = {
      val mbc = fb.composeWithMonad[C]
      val run1 = fa.map(run)(bcz => mbc.bind(bcz)(f))
      new MonadWrapper3[A, B, C, W](run1)
    }

    def map4[W](f: Z => A[B[C[W]]])(implicit ma: Monad[A], mb: Monad[B], mc: Monad[C], tc: Traverse[C], tb: Traverse[B]): MonadWrapper3[A, B, C, W] = {
      type AB[T] = A[B[T]]
      val mab: Monad[AB] = ma.composeWithMonad[B]
      val mabc = new MonadicComposerOps(mab).composeWithMonad[C]
      val run1 = mabc.bind(run)(f)
      new MonadWrapper3[A, B, C, W](run1)
    }

    def flatMap[W](f: Z => MonadWrapper3[A, B, C, W])(implicit ma: Monad[A], mb: Monad[B], mc: Monad[C], tc: Traverse[C], tb: Traverse[B]): MonadWrapper3[A, B, C, W] =
      map4(f.andThen(_.run))
  }

  class MonadWrapper2[A[_], B[_], Z](val run: A[B[Z]]) {

    def up = new MonadWrapper1[A, B[Z]](run)

    def down[C[_], W](implicit fa: Functor[A], fb: Functor[B], ev: Z =:= C[W]): MonadWrapper3[A, B, C, W] = {
      val res = fa.compose[B].map(run)(ev)
      new MonadWrapper3[A, B, C, W](res)
    }

    def apply[W](f: Z => W)(implicit fa: Functor[A], fb: Functor[B]): A[B[W]] = map(f).run

    def map[W](f: Z => W)(implicit fa: Functor[A], fb: Functor[B]): MonadWrapper2[A, B, W] = {
      val run1 = fa.compose[B].map(run)(f)
      new MonadWrapper2(run1)
    }

    def map2[W](f: Z => B[W])(implicit fa: Functor[A], mb: Monad[B]): MonadWrapper2[A, B, W] = {
      val run1 = fa.map(run)(bz => mb.bind(bz)(f))
      new MonadWrapper2(run1)
    }

    def map3[W](f: Z => A[B[W]])(implicit fa: Monad[A], mb: Monad[B], tb: Traverse[B]): MonadWrapper2[A, B, W] = {
      val mab = fa.composeWithMonad[B]
      val run1 = mab.bind(run)(f)
      new MonadWrapper2(run1)
    }

    def flatMap[W](f: Z => MonadWrapper2[A, B, W])(implicit fa: Monad[A], mb: Monad[B], tb: Traverse[B]): MonadWrapper2[A, B, W] = {
      val mab = fa.composeWithMonad[B]
      val run1 = mab.bind(run)(a => f(a).run)
      new MonadWrapper2(run1)
    }
  }

  class MonadWrapper1[A[_], Z](val run: A[Z]) {

    def down[B[_], W](implicit f: Functor[A], ev: Z =:= B[W]): MonadWrapper2[A, B, W] = {
      new MonadWrapper2[A, B, W](f.map(run)(ev.apply))
    }

    def apply[W](f: Z => W)(implicit fa: Functor[A]): A[W] = map(f).run


    def map[W](f: Z => W)(implicit fa: Functor[A]): MonadWrapper1[A, W] = {
      val run1 = fa.map(run)(f)
      new MonadWrapper1[A, W](run1)
    }

    def map2[W](f: Z => A[W])(implicit ma: Monad[A]): MonadWrapper1[A, W] = {
      val run1 = ma.bind(run)(f)
      new MonadWrapper1[A, W](run1)
    }

    def flatMap[W](f: Z => MonadWrapper1[A, W])(implicit ma: Monad[A]): MonadWrapper1[A, W] = {
      new MonadWrapper1[A, W](ma.bind(run)(a => f(a).run))
    }
  }
}

