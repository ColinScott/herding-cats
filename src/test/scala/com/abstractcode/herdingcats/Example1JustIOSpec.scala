package com.abstractcode.herdingcats

import cats.effect.IO
import org.specs2.matcher.MatchResult
import org.specs2.scalacheck.{ScalaCheckFunction1, ScalaCheckFunction2}
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class Example1JustIOSpec extends Specification with ScalaCheck {
  def is: SpecStructure =
    s2"""
         We can easily test we got the thing $gotTheThing

         Little harder to find out if we cleaned up $didCleanup

         How do we ensure we didn't clean up before we got the thing? $checkOrdering
      """


  def gotTheThing: ScalaCheckFunction2[ThingKey, TheThing, MatchResult[Any]] =
    prop {
      (key: ThingKey, thing: TheThing) => {
        val getTheThing: ThingKey => IO[TheThing] = {
          case `key` => IO.pure(thing)
          case _ => IO.raiseError(new Exception("Got the wrong key"))
        }

        val result = SimpleExample.example(getTheThing)(_ => IO.unit)(key).attempt.unsafeRunSync()

        result shouldEqual Right(thing.value)
      }
    }

  def didCleanup: ScalaCheckFunction2[ThingKey, TheThing, MatchResult[Boolean]] =
    prop {
      (key: ThingKey, thing: TheThing) => {
        var calledCleanup = false // Wait, what's this?

        val getTheThing: ThingKey => IO[TheThing] = _ => IO.pure(thing)

        val cleanUpTheThing: ThingKey => IO[Unit] = {
          case `key` =>
            calledCleanup = true // This is terrible
            IO.unit
          case _ => IO.unit
        }

        SimpleExample.example(getTheThing)(cleanUpTheThing)(key).attempt.unsafeRunSync()

        calledCleanup should beTrue // Just using side effects to test functional code. <this is fine.gif>
      }
    }

  def checkOrdering: ScalaCheckFunction1[ThingKey, MatchResult[Boolean]] =
    prop {
      (key: ThingKey) => {
        var calledCleanup = false // This again?

        val getTheThing: ThingKey => IO[TheThing] = _ => IO.raiseError(new Exception("Getting the thing failed"))

        val cleanUpTheThing: ThingKey => IO[Unit] = _ => {
          calledCleanup = true // This is terrible
          IO.unit
        }

        SimpleExample.example(getTheThing)(cleanUpTheThing)(key).attempt.unsafeRunSync()

        // So we're just going to ignore the failure we induced?

        calledCleanup should beFalse // Better hope nobody ever makes things async
      }
    }
}
