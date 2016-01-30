package cbor

import java.io.ByteArrayOutputStream

import co.nstant.in.cbor.{CborBuilder, CborEncoder}

/**
  * Created by ytaras on 1/30/16.
  */
object TestModel {
  def serialize(x: List[CborTree]): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val encoder = new CborEncoder(baos)
    val builder = new CborBuilder()
    x.foreach(_.build(builder))
    encoder.encode(builder.build())
    baos.toByteArray
  }


  sealed trait CborTree {
    def build(encoder: CborBuilder)
  }

  case class Integer(i: Int) extends CborTree {
    override def build(encoder: CborBuilder): Unit =
      encoder.add(i)
  }

}
