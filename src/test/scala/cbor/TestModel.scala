package cbor

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import co.nstant.in.cbor.model.DataItem
import co.nstant.in.cbor.{CborBuilder, CborDecoder, CborEncoder}
import scodec.{Attempt, DecodeResult}

import scala.collection.JavaConversions._

/**
  * Created by ytaras on 1/30/16.
  */
object TestModel {
  def serialize(x: CborValue): Array[Byte] = {
    serialize(List(x))
  }

  def serialize(x: List[CborValue]): Array[Byte] = {
    withBuilder(builder => x.foreach(_.build(builder)))
  }

  def withBuilder(u: CborBuilder => Unit): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val encoder = new CborEncoder(baos)
    val builder = new CborBuilder()
    u(builder)
    encoder.encode(builder.build())
    baos.toByteArray
  }

  def deserialize(x: Array[Byte]): Seq[DataItem] = {
    new CborDecoder(new ByteArrayInputStream(x)).decode()
  }

  implicit class ResultOps[V](inner: Attempt[DecodeResult[V]]) {
    def get: V = inner.toOption.get.value
  }

  implicit class CborValueOps(inner: CborValue) {
    def build(builder: CborBuilder): Unit = inner match {
      case CInteger(i) => builder.add(i.bigInteger)
    }
  }
}

