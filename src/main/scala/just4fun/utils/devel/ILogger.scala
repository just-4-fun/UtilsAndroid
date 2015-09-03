package just4fun.utils.devel

import scala.language.implicitConversions

object ILogger {
	implicit def string2exception(msg: String): Exception = WrappedMessage(msg)

	private[this] val VERBOSE = 2
	private[this] val DEBUG = 3
	private[this] val INFO = 4
	private[this] val WARN = 5
	private[this] val ERROR = 6
	private[this] val ASSERT = 7
	
	private[this] var _debug = true
	private[this] var _tagPrefix = getClass.getPackage.getName.takeWhile(_ != '.')
	private[this] var _logDef: (Int, String, String) => Unit = (level, tag, message) => println(s"[$tag] ::  $message")
	private[this] var _delegateErrorDef: (Throwable, String, String) => Unit = null

	def isDebug: Boolean = _debug
	def debug(yes: Boolean): this.type = { _debug = yes; this }
	def tagPrefix(v: String): this.type = { _tagPrefix = v; this }
	def logDef(f: (Int, String, String) => Unit): this.type = { _logDef = f; this }
	def delegateErrorDef(f: (Throwable, String, String) => Unit): this.type = { _delegateErrorDef = f; this }

	def logV(msg: => String, enclosingMethod: String = null)(implicit tag: LogTag = null) = if (_debug) {
		val what = if (enclosingMethod == null) msg else s"[$enclosingMethod] :: $msg"
		val where = if (tag == null) _tagPrefix else _tagPrefix + "." + tag.name
		_logDef(VERBOSE, where, what)
	}
	def logD(msg: => String, enclosingMethod: String = null)(implicit tag: LogTag = null) = if (_debug) {
		val what = if (enclosingMethod == null) msg else s"[$enclosingMethod] :: $msg"
		val where = if (tag == null) _tagPrefix else _tagPrefix + "." + tag.name
		_logDef(DEBUG, where, what)
	}
	def logI(msg: => String, enclosingMethod: String = null)(implicit tag: LogTag = null) = if (_debug) {
		val what = if (enclosingMethod == null) msg else s"[$enclosingMethod] :: $msg"
		val where = if (tag == null) _tagPrefix else _tagPrefix + "." + tag.name
		_logDef(INFO, where, what)
	}
	def logW(msg: => String, enclosingMethod: String = null)(implicit tag: LogTag = null) = if (_debug) {
		val what = if (enclosingMethod == null) msg else s"[$enclosingMethod] :: $msg"
		val where = if (tag == null) _tagPrefix else _tagPrefix + "." + tag.name
		_logDef(WARN, where, what)
	}
	def logE(error: => Throwable, msg: => String = null)(implicit tag: LogTag = null) = if (_debug || _delegateErrorDef != null) {
		val where = if (tag == null) _tagPrefix else _tagPrefix + "." + tag.name
		val what = error match {
			case null => msg
			case WrappedMessage(m) => m
			case _ => val builder = new StringBuilder
				if (msg != null) builder ++= msg ++= "\n"
				val pkgRoot = if (tag == null) _tagPrefix else tag.pkgRoot
				stackTrace(error, builder, pkgRoot).toString()
		}
		if (_delegateErrorDef == null) _logDef(ERROR, where, what)
		else _delegateErrorDef(error, where, what)
	}

	def stackTrace(error: Throwable, builder: StringBuilder, packageRoot: String): StringBuilder = {
		var msg = error.toString
		if (msg == null || msg.isEmpty) msg = error.getClass.getName + ": " + error.getMessage
		builder ++= "    " ++= msg ++= "\n"
		val stack = error.getStackTrace
		var root = false
		var n = 0
		var next = n < stack.length
		while (next) {
			val elt = stack(n)
			builder ++= "    " ++= elt.toString ++= "\n"
			n += 1
			val _root = elt.getClassName.startsWith(packageRoot)
			next = n < stack.length && (!root || _root)
			root = _root
		}
		if (n < stack.length) builder ++= "    ... " ++= (stack.length - n).toString ++= " more stack records."
		val cause = error.getCause
		if (cause != null && error != cause) stackTrace(error.getCause, builder ++= "\n    ... Caused by: \n", packageRoot)
		builder
	}

	/* CLASSES */

	trait Loggable {
		implicit lazy val logtag = LogTag(this)
	}


	case class LogTag(thisOrName: AnyRef) {
		val name = thisOrName match {
			case s: String => s
			case _ => thisOrName.getClass.getSimpleName
		}
		lazy val pkgRoot = thisOrName match {
			case s: String => _tagPrefix
			case _ => thisOrName.getClass.getPackage.getName
		}
		override def toString: String = name
	}


	case class WrappedMessage(message: String) extends Exception(message)

}
