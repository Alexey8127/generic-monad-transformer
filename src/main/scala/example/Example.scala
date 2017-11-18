package example

import utils.scalaz.GenericMonadTransformer._

import scalaz._
import Scalaz._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Example extends App {

  type EitherString[A] = String \/ A

  val nestedMonads: Future[EitherString[Option[List[Int]]]] = List(1,2,3).some.right[String].pure[Future]

  val result =
    nestedMonads
      .composeMonads4
      .map(_ * 3)
      .up
      .map(_.filter(_ == 4).sum)
      .up
      .up
      .map(_.fold(identity, _.map(_.toString).getOrElse("oops!")))
      .map2(_ => Future.successful(20))
      .run

  result.onComplete(println)
}
