package cbor

import org.scalacheck.Properties

/**
  * Created by ytaras on 1/31/16.
  */
class CodecsSpecification extends Properties("Codecs") {
  //  property("Generates arbitrary") = forAll { (x: CborTree) =>
  //    !x.toString.isEmpty
  //  }
  //  property("Parses major type") = forAll { (x: CborTree) =>
  //    val bytes = ByteVector(serialize(x))
  //    Codec.decode(bytes.toBitVector)(Codecs.majorType) == Tags.UnsignedInteger
  //  }

}
