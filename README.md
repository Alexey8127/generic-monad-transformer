# generic-monad-transformer
Generic monad transformer using scalaz library

Simple usage example of generic monad transformer with 4 nested monads:

```scala

object Example extends App {

  type EitherString[A] = String \/ A

  val nestedMonads: Future[EitherString[Option[List[Int]]]] = List(1,2,3).some.right[String].pure[Future]

  val result =
    nestedMonads
      .composeMonads4 // MonadWrapper4[Future, EitherString, Option, List, Int]
      .map(_ * 3) // MonadWrapper4[Future, EitherString, Option, List, Int]
      .up // MonadWrapper3[Future, EitherString, Option, List[Int]]
      .map(_.filter(_ == 4).sum) // MonadWrapper3[Future, EitherString, Option, Int]
      .up // MonadWrapper2[Future, EitherString, Option[Int]]
      .up // MonadWrapper1[Future, EitherString[Option[Int]]]
      .map(_.fold(identity, _.map(_.toString).getOrElse("oops!"))) // MonadWrapper1[Future, String]
      .map2(_ => Future.successful(20)) // MonadWrapper1[Future, Int] 
      .run // Future[Int]

  result.onComplete(println)
}
```
