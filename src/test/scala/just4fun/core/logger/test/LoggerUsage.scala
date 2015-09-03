package just4fun.core.logger.test

import just4fun.utils.devel.ILogger
import ILogger._

object LoggerUsage extends App with Loggable{
	ILogger
	  .logDef((lvl, tag, msg)=> println(s"$tag>  $msg"))
	  .delegateErrorDef((err, tag, msg) => println(s"ERROR >> $tag>  $msg"))
	  .debug(false)
	  .debug(true)

	logI("Hao :)", "Test")
	logI("Just Hao :)")
	logI(null)
	try { 3/0}
	catch {
		case e: Throwable =>
			logE(e, "Ooops..")
			logE(e)
			logE("Never divide by 0")
			logE(null)
	}

}
