package llc4

object Printer {
  def print(ast: JsAst, indent: Int): String = {
    def p(ast: JsAst) = print(ast, indent)
    def p2(ast: JsAst) = " " * (indent + 2) + print(ast, indent + 2)
    ast match {
      case JsBooleanLiteral(value)                  => value.toString
      case JsStringLiteral(value)                   => value
      case JsNumericLiteral(value, _)               => value.toString
      case JsIdentifierExpr(value)                  => value
      case JsUnOpExpr(operator, operand)            => s"${operator}{p(operand)}"
      case JsBinOpExpr(operator, lhs, rhs)          => s"${p(lhs)} ${operator} ${p(rhs)}"
      case JsCallExpr(callee, params)               => s"""${p(callee)}(${params.map(p(_)).mkString(", ")})"""
      case JsEmptyStmt                              => ""
      case JsBlockStmt(stmts)                       => s"""{\n${stmts.map(p2(_)).mkString(";\n")}\n${" " * indent}}"""
      case JsExprStmt(jsExpr)                       => p(jsExpr)
      case JsIfStmt(cond, thenp, elsep)             => s"if (${p(cond)}) ${p(thenp)}" + elsep.map(e => s" else ${p(e)}").getOrElse("")
      case JsWhileStmt(cond, body)                  => s"while (${p(cond)}) ${p(body)}"
      case JsVarDefStmt(identifier, initializer, _) => s"var ${identifier} = ${p(initializer)}"
      case JsFunDeclStmt(identifier, params, body)  => s"""function ${identifier}(${params.mkString(", ")}) ${p(body)}"""
      case JsReturnStmt(jsExpr)                     => s"return ${p(jsExpr)}"
    }
  }
}
