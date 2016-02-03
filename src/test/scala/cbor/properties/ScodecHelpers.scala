package cbor.properties

import org.scalatest.enablers.Containing
import scodec.bits._
import scodec.codecs._
import scodec.{Attempt, Codec, DecodeResult}
import shapeless.Poly1

import scala.language.higherKinds

/**
  * Created by ytaras on 2/3/16.
  */
trait ScodecHelpers {

  implicit def attemptContaining[A] = new Containing[Attempt[A]] {
    override def contains(container: Attempt[A], element: Any): Boolean =
      container.toOption.contains(element)

    override def containsOneOf(container: Attempt[A], elements: Seq[Any]): Boolean = ???

    override def containsNoneOf(container: Attempt[A], elements: Seq[Any]): Boolean = ???
  }

  implicit def attemptResultContaining[A](implicit out: Containing[Attempt[A]]) = new Containing[Attempt[DecodeResult[A]]] {
    override def contains(container: Attempt[DecodeResult[A]], element: Any): Boolean =
      out.contains(container.map(_.value), element)

    override def containsOneOf(container: Attempt[DecodeResult[A]], elements: Seq[Any]): Boolean =
      out.containsOneOf(container.map(_.value), elements)

    override def containsNoneOf(container: Attempt[DecodeResult[A]], elements: Seq[Any]): Boolean =
      out.containsNoneOf(container.map(_.value), elements)
  }

  def decodeByNumber[A](c: Codec[A])(b: Array[Byte]) =
    decodedWithPrefix(bin"000")(c, b)

  def decodedWithPrefix[A](p: BitVector)(c: Codec[A], b: Array[Byte]) = {
    val codec = constant(p) ~> c
    decode(codec)(b)
  }

  def decode[A](c: Codec[A])(b: Array[Byte]) = {
    val data = ByteVector(b).toBitVector
    Codec.decode(data)(c)
  }

  def decodeByNegativeNumber[A](c: Codec[A])(b: Array[Byte]) =
    decodedWithPrefix(bin"001")(c, b)

  object toNum extends Poly1 {
    implicit def caseNum[N: Numeric] = at[N](identity)
  }

}
