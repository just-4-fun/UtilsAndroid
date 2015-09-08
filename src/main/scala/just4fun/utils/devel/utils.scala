package just4fun.utils.devel

import just4fun.utils.logger.Logger._


object Utils {

//	def printObject[T <: AnyRef](v : T)(implicit schema: SchemaType[T] = null): String = {
//		val sch = if (schema == null )PropType.genSchema[T] else schema
//		sch.valuesTo(v)(JsonArrayFactory)
//	}

}

object measureTime {
//	import just4fun.utils.devel.ILogger._
	def apply[T](tag: String, times: Int = 1)(code: => T): T = {
		var result: T = null.asInstanceOf[T]
		var ns = 0L
		code
		code
		if (times == 1) {
			val t0 = System.nanoTime()
			result = code
			ns = System.nanoTime() - t0
			logI(s"$tag TIME= $ns")
//			logI(s"$tag TIME= $ns")
		}
		else {
			val range = 0 until times
			val t0 = System.nanoTime()
			for (_ <- range) result = code
			ns = System.nanoTime() - t0
			logI(s"$tag TIME (ms per $times; ns per 1)=  ${(ns / 1000000f).toInt};   ${ns / times}")
//			logI(s"$tag TIME (ms per $times; ns per 1)=  ${(ns / 1000000f).toInt};   ${ns / times}")
		}
		result
	}
}


