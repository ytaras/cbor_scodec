package cbor

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import org.scalacheck.Shapeless._

/**
  * Created by ytaras on 1/30/16.
  */
class TestModelSpecification extends Properties("TestModel") {
  property("Generates arbitrary") = forAll { (x: TestModel.CborTree) =>
    !x.toString.isEmpty
  }

}
