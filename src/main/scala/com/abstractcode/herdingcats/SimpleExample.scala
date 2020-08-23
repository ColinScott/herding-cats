package com.abstractcode.herdingcats

import cats.implicits._
import cats.Monad

case class ThingKey(key: String)
case class TheThing(value: Int)

object SimpleExample {
  def example[F[_] : Monad](getTheThing: ThingKey => F[TheThing])(cleanUpTheThing: ThingKey => F[Unit])(key: ThingKey): F[Int] = for {
    theThing <- getTheThing(key)
    _ <- cleanUpTheThing(key)
  } yield theThing.value
}
