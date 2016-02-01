package cbor.properties

import cbor.TestModel._
import cbor._
import co.nstant.in.cbor.CborBuilder
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.Properties
import org.scalacheck.Shapeless._
import shapeless.ops.coproduct.{Length => CoproductLength}
import shapeless.ops.hlist.{Length => HListLength}

import scala.collection.JavaConversions._

/**
  * Created by ytaras on 1/30/16.
  */
class TestModelP extends Properties("TestModel") {
  property("Generates arbitrary") = forAll { (x: CborTree) =>
    !x.toString.isEmpty
  }
  property("Converts to byte array") = forAll { (x: List[CborTree]) =>
    x.nonEmpty ==> !serialize(x).isEmpty
  }
  property("Serializes to correct byte array") = forAll { (x: List[CborTree]) =>
    val items = deserialize(serialize(x))
    items.size == x.size && x.zip(items).forall { case (model, di) => model.matches(di) }
  }
  property("Serializes to correct data item") = forAll { (x: CborTree) =>
    val builder = new CborBuilder()
    x.build(builder)
    val Seq(di) = builder.build().toSeq
    x.matches(di)
  }
}

