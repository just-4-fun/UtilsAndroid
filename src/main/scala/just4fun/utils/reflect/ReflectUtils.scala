package just4fun.utils.reflect

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.util.matching.Regex

object ReflectUtils {
	lazy val OptionRx = """Option\[.*""".r

	def construct[T](clas: Class[T]): T = macro constructImpl[T]

	/* IMPLs */

	def constructImpl[T: c.WeakTypeTag](c: Context)(clas: c.Tree): c.Tree = {
		import c.universe._
		implicit val _c = c
		// DEFs
		def genConstructor(instT: Type, constrSym: MethodSymbol): String = {
			val newOp = if (instT.typeSymbol.asClass.isCaseClass) "" else "new "
			val text = new StringBuilder
			constrSym.paramLists.foreach { params =>
				val pps = params.withFilter { p => !p.asTerm.isParamWithDefault && !p.asTerm.isImplicit }.map(p => s"${p.name}=${defaultValue(p.info.toString)}").mkString(",")
				text.append("(")
				if (pps.length > 0) text.append(pps)
				text.append(")")
			}
			//		print(s"override def $INSTANCE_x = $newOp${pearlT.typeSymbol.fullName}$text\n")
			s"$newOp${instT.typeSymbol.fullName}$text\n"
		}
		def constructorSym(instT: Type) = instT.members.find(s => s.isConstructor && s.asMethod.isPrimaryConstructor) match {
			case Some(s) => s.asMethod
			case None => c.abort(c.enclosingPosition, s"Constructor for class ${instT} not found.")
		}
		def defaultValue(typ: String): String = typ match {
			case "String" => "null"
			case "Long" | "Int" | "Double" | "Float" | "Short" | "Byte" | "Char" => "0"
			case "Boolean" => "false"
			case OptionRx() => "None"
			case _ => "null"
		}
		// EXEC
		val instT = weakTypeOf[T]
		prn(s"INST>  $instT")
		val consSym = constructorSym(instT)
		prn(s"SYM>  $consSym")
		val cons = genConstructor(instT, consSym)
		prn(s"CONSTR>  $cons")
		c.parse(cons)
	}
	def prn(text: String)(implicit c: Context) = c.info(c.enclosingPosition, text, false)
}
