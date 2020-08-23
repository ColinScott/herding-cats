package com.abstractcode.herdingcats

import cats.effect.IO
import cats.effect.concurrent.Ref
import org.specs2.matcher.MatchResult
import org.specs2.scalacheck.ScalaCheckFunction2
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class Example3RefSpec extends Specification with ScalaCheck {
  def is: SpecStructure =
    s2"""
         We can easily test we got the thing $gotTheThing

         Let's test we did all the things $didAllTheThings
      """

  def gotTheThing: ScalaCheckFunction2[ThingKey, TheThing, MatchResult[Any]] =
    prop {
      (key: ThingKey, thing: TheThing) => {
        val getTheThing: ThingKey => IO[TheThing] = {
          case `key` => IO.pure(thing)
          case _ => IO.raiseError(new Exception("Got the wrong key"))
        }

        val result = SyncExample.example(getTheThing)(_ => IO.unit)(key).attempt.unsafeRunSync()

        result shouldEqual Right(thing.value)
      }
    }

  def didAllTheThings: ScalaCheckFunction2[ThingKey, TheThing, MatchResult[Any]] =
    prop {
      (key: ThingKey, thing: TheThing) => {
        def getTheThing(ref: Ref[IO, List[String]]): ThingKey => IO[TheThing] = {
          case `key` => for {
            t <- IO.pure(thing)
            _ <- ref.modify(state => ("got key" :: state, ()))
          } yield t
          case _ => IO.raiseError(new Exception("Got the wrong key"))
        }

        def cleanUpTheThing(ref: Ref[IO, List[String]]): ThingKey => IO[Unit] = {
          case `key` => ref.modify(state => ("cleanup" :: state, ()))
          case _ => IO.unit
        }

        val program = for {
          ref <- Ref.of[IO, List[String]](Nil)
          _ <- SyncExample.example(getTheThing(ref))(cleanUpTheThing(ref))(key)
          value <- ref.get
        } yield value

        val result = program.attempt.unsafeRunSync()

        result shouldEqual Right(List("cleanup", "got key")) // Test that the operations happened in the order we need
      }
    }

}
