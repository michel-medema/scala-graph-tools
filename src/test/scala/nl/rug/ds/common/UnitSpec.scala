package nl.rug.ds.common

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

abstract class UnitSpec
	extends AnyFlatSpec with should.Matchers with OptionValues with Inside with Inspectors with GivenWhenThen