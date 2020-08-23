package com.abstractcode

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}

package object herdingcats {
  implicit val arbitraryKey: Arbitrary[ThingKey] = Arbitrary(Gen.identifier.map(ThingKey))
  implicit val arbitraryThing: Arbitrary[TheThing] = Arbitrary(arbitrary[Int].map(TheThing))

}
