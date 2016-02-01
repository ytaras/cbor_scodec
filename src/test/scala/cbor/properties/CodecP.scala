package cbor.properties

import cbor.TestModel._
import cbor._
import cbor.coded.{Codecs, MajorTypes}
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.Properties
import org.scalacheck.Shapeless._
import shapeless.ops.coproduct.{Length => CoproductLength}
import shapeless.ops.hlist.{Length => HListLength}

/**
  * Created by ytaras on 1/30/16.
  */
class CodecP extends Properties("Codec") {


  property("Parses integer type") = forAll {
    (x: BigInteger) => (x.i >= 0) ==> {
      x.codeDecode(Codecs.majorType).get == MajorTypes.UnsignedInteger
    }
  }
  property("Parses small int value") = forAll {
    (x: BigInteger) => (x.i >= 0 && x.i < 24) ==> {
      x.codeDecode(Codecs.typeAndValue).get ==(MajorTypes.UnsignedInteger, x.i)
    }
  }
  property("Parses negative integer type") = forAll {
    (x: BigInteger) => (x.i < 0) ==> {
      x.codeDecode(Codecs.majorType).get == MajorTypes.NegativeInteger
    }
  }
  property("Parses negative integer value") = forAll {
    (x: BigInteger) => (x.i < 0 && x.i > -24) ==> {
      x.codeDecode(Codecs.typeAndValue).get ==(MajorTypes.NegativeInteger, x.i)
    }
  }
}

