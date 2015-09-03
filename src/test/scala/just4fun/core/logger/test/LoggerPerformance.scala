package just4fun.core.logger.test

import just4fun.utils.devel.{ILogger, measureTime}
import ILogger._

// MEASURE PERFORMANCE

object LoggerPerformance extends App with Loggable{
	val error: Exception = new Exception("error Just Hao :)")

	// 50.000
	def t1 = measureTime("                                   Log Info msg") {
		logI("logI Just Hao :)")
	}
	// 60.000
	def t2 = measureTime("                                   Log Error msg") {
		logE("logE Just Hao :)")
	}
	// 130.000
	def t3 = measureTime("                                   Log Error object") {
		logE(error)
	}
	// 30.000
	def t4 = measureTime("                                   Print") {
		println("println Just Hao :)")
	}

	logW("true *****************************", "DEBUG")
	ILogger
	  .logDef((lvl, tag, msg)=> println(s"$tag>  $msg"))
	  .delegateErrorDef((err, tag, msg) => println(s"ERROR >> $tag>  $msg"))
	  .debug(true)

	t4;t3;t2;t1;
	t4;t3;t2;t1;
	t4;t3;t2;t1;
	t4;t3;t2;t1;

	logW("false *****************************", "DEBUG")
	ILogger
	  .debug(false)

	t4;t3;t2;t1;
}
