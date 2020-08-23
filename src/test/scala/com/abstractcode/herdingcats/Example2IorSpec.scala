package com.abstractcode.herdingcats

import cats.data.Ior
import cats.implicits._
import org.specs2.matcher.MatchResult
import org.specs2.scalacheck.ScalaCheckFunction2
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class Example2IorSpec extends Specification with ScalaCheck {

  type Test[A] = Ior[List[String], A] // Let's define a type to help

  def is: SpecStructure =
    s2"""
         We can easily test we got the thing $gotTheThing

         Let's test we did all the things $didAllTheThings
      """

  def gotTheThing: ScalaCheckFunction2[ThingKey, TheThing, MatchResult[Any]] =
    prop {
      (key: ThingKey, thing: TheThing) => {
        val getTheThing: ThingKey => Test[TheThing] = {
          case `key` => thing.rightIor
          case _ => List("Got the wrong key").leftIor
        }

        val result = SimpleExample.example(getTheThing)(_ => ().rightIor)(key)

        result shouldEqual thing.value.rightIor // Test that we got the right value out
      }
    }

  def didAllTheThings: ScalaCheckFunction2[ThingKey, TheThing, MatchResult[Any]] =
    prop {
      (key: ThingKey, thing: TheThing) => {
        val getTheThing: ThingKey => Test[TheThing] = {
          case `key` => Ior.both(List("got key"), thing) // Left must be a Semigroup so this state will be combined automatically
          case _ => List("Got the wrong key").leftIor
        }

        val cleanUpTheThing: ThingKey => Test[Unit] = {
          case `key` => Ior.both(List("cleanup"), ())
          case _ => List("wrong cleanup key").leftIor
        }

        val result = SimpleExample.example(getTheThing)(cleanUpTheThing)(key)

        result.left shouldEqual Some(List("got key", "cleanup")) // Test that the operations happened in the order we need
      }
    }
}
