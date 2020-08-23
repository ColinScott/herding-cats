package com.abstractcode.herdingcats

import cats.effect.Sync
import cats.implicits._

object SyncExample {
  def example[F[_] : Sync](getTheThing: ThingKey => F[TheThing])(cleanUpTheThing: ThingKey => F[Unit])(key: ThingKey): F[Int] = for {
    theThing <- getTheThing(key)
    _ <- Sync[F].pure(()) // What this is doesn't matter for this example
    _ <- cleanUpTheThing(key)
  } yield theThing.value
}
