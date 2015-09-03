package just4fun.utils

import scala.collection.GenTraversableOnce

object Utils {
	def isEmpty(value: Any): Boolean = {
//		print(value + "  ")
		value match {
			case null => true
			case v: String => v.isEmpty
			case v: Option[_] => v.isEmpty || isEmpty(v.get)
			case v: GenTraversableOnce[_] => v.isEmpty
			case v: Array[_] => v.length == 0
			case v: Int => v == 0
			case v: Long => v == 0
			case v: Double => v == 0
			case v: Float => v == 0
			case v: Short => v == 0
			case v: Byte => v == 0
			case v: Char => v == 0
			case v: Boolean => !v
			case v: Unit => true
			case _ => false
		}
	}
	def nonEmpty(value: Any): Boolean = !isEmpty(value)

	// TIME
	def now = System.currentTimeMillis

	// MISC

}


//object TryNLog {
//	def apply[T](codeBlock: => T)(implicit tag: LogTag): Try[T] = {
//		Try { codeBlock } match {
//			case f@Failure(e) => logE(e)(tag); f
//			case res => res
//		}
//	}
//}
//object TryNClose {
//	def apply[U <: {def close()}, R](r: U)(f: U => R): Try[R] = {
//		try {Success(f(r))} catch {case e: Throwable => Failure(e) }
//		finally {try {if (r != null) r.close()} catch {case _: Throwable => () } }
//	}
//}

