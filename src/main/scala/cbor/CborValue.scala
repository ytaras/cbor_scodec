package cbor

import scodec.codecs.~

/**
  * Created by ytaras on 2/2/16.
  */
sealed trait CborValue

case class CInteger(i: BigInt) extends CborValue {
}

object CInteger {
  def apply(x: ~[Long, Long]): CInteger = x match {
    case one ~ two =>
      CInteger((BigInt(one) << 32) + two)
  }
}

//case class CString(s: String) extends CborValue
