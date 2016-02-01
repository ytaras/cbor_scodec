package cbor

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import co.nstant.in.cbor.model.{DataItem, Number}
import co.nstant.in.cbor.{CborBuilder, CborDecoder, CborEncoder}

import scala.collection.JavaConversions._

/**
  * Created by ytaras on 1/30/16.
  */
object TestModel {
  def serialize(x: CborTree): Array[Byte] = {
    serialize(List(x))
  }
  def serialize(x: List[CborTree]): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val encoder = new CborEncoder(baos)
    val builder = new CborBuilder()
    x.foreach(_.build(builder))
    encoder.encode(builder.build())
    baos.toByteArray
  }

  def deserialize(x: Array[Byte]): Seq[DataItem] = {
    new CborDecoder(new ByteArrayInputStream(x)).decode()
  }


}

sealed trait CborTree {
  def build(encoder: CborBuilder)

  def matches(di: DataItem): Boolean
}

case class BigInteger(i: Int) extends CborTree {
  override def build(encoder: CborBuilder): Unit =
    encoder.add(i)

  override def matches(di: DataItem): Boolean = di match {
    case din: Number => din.getValue.intValue() == i
    case _ => false

  }
}
