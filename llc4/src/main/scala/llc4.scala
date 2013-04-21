package llc4

import language.experimental.macros

import scala.reflect.macros.Context

sealed trait JsAst

sealed trait JsStmt extends JsAst
sealed trait JsExpr extends JsAst

case class JsBooleanLiteral(value: Boolean) extends JsExpr
case class JsStringLiteral(value: String) extends JsExpr
case class JsNumericLiteral(value: Double, isFloat: Boolean) extends JsExpr

case class JsIdentifierExpr(identifier: String) extends JsExpr
case class JsUnOpExpr(operator: String, operand: JsExpr) extends JsExpr
case class JsBinOpExpr(operator: String, lhs: JsExpr, rhs: JsExpr) extends JsExpr
case class JsCallExpr(callee: JsExpr, params: List[JsExpr]) extends JsExpr

case object JsEmptyStmt extends JsStmt
case class JsBlockStmt(stmts: List[JsStmt]) extends JsStmt
case class JsExprStmt(jsExpr: JsExpr) extends JsStmt
case class JsIfStmt(cond: JsExpr, `then`: JsStmt, `else`: Option[JsStmt]) extends JsStmt
case class JsWhileStmt(cond: JsExpr, body: JsStmt) extends JsStmt
case class JsVarDefStmt(identifier: String, initializer: JsExpr, isConst: Boolean) extends JsStmt
case class JsFunDeclStmt(identifier: String, params: List[String], body: JsStmt) extends JsStmt

package object js {
  def js(expr: Any): JsAst = macro js_impl
  def js_impl(c: Context)(expr: c.Expr[Any]) = {
    import c.universe._

    type ToExpr[T] = PartialFunction[Tree, Expr[T]]

    object UnaryOperator {
      val ops = Set("~", "+", "-", "!")
      val decodedToOp = ops.map(op => newTermName("unary_" + op).encodedName -> op).toMap.lift
      def unapply(tree: Tree): Option[(String, Tree)] = Some(tree).collect {
        case Select(qualifier, name) => decodedToOp(name).map(_ -> qualifier)
      }.flatten
    }

    object BinaryOperator {
      val ops = Set(
        "*", "/", "%",
        "+", "-",
        "<<", ">>", ">>>",
        "<", ">", "<=", ">=",
        "==", "!=", "===", "!==",
        "&", "^", "|",
        "&&", "||"
      )
      val decodedToOp = ops.map(op => newTermName(op).encodedName -> op).toMap.lift
      def unapply(tree: Tree): Option[(String, Tree, Tree)] = Some(tree).collect {
        case Apply(Select(lhs, name), List(rhs)) => decodedToOp(name).map((_, lhs, rhs))
      }.flatten
    }

    object Assignment {
      val ops = Set(
        "*", "/", "%",
        "+", "-",
        "<<", ">>", ">>>",
        "&", "^", "|"
      )
      val decodedToOp = ops.map(op => newTermName(op).encodedName -> (op + "=")).toMap.lift
      def unapply(tree: Tree) = Some(tree).collect {
        case Assign(lhs, Apply(Select(lhs0, name), List(rhs))) if lhs.equalsStructure(lhs0) => decodedToOp(name).map((_, lhs, rhs))
        case Assign(lhs, rhs) => Some(("=", lhs, rhs))
      }.flatten
    }

    def isUnitLiteral(tree: Tree): Boolean = tree.equalsStructure(c.literalUnit.tree)

    def listOfExprToExprOfList[T](exprs: List[Expr[T]]): Expr[List[T]] = c.Expr[List[T]](treeBuild.mkMethodCall(reify(List).tree, exprs.map(_.tree)))

    lazy val toJsBooleanLiteral      : ToExpr[JsBooleanLiteral] = { case Literal(Constant(value: Boolean)) => reify(JsBooleanLiteral(c.literal(value).splice)) }
    lazy val toJsStringLiteral       : ToExpr[JsStringLiteral]  = { case Literal(Constant(value: String))  => reify(JsStringLiteral (c.literal(value).splice)) }
    lazy val intToJsStringLiteral    : ToExpr[JsNumericLiteral] = { case Literal(Constant(value: Int))     => reify(JsNumericLiteral(c.literal(value).splice, false)) }
    lazy val doubleToJsStringLiteral : ToExpr[JsNumericLiteral] = { case Literal(Constant(value: Double))  => reify(JsNumericLiteral(c.literal(value).splice, true)) }

    lazy val toJsIdentifierExpr: ToExpr[JsIdentifierExpr] = {
      case Ident(name) =>
        val identifier = c.literal(name.encoded)
        reify(JsIdentifierExpr(identifier.splice))
    }

    lazy val toJsUnOpExpr: ToExpr[JsUnOpExpr] = {
      case UnaryOperator(operator, operand) =>
        reify(JsUnOpExpr(c.literal(operator).splice, toJsExpr(operand).splice))
    }

    lazy val toJsBinOpExpr: ToExpr[JsBinOpExpr] = {
      case BinaryOperator(op, lhs, rhs) =>
        reify(JsBinOpExpr(c.literal(op).splice, toJsExpr(lhs).splice, toJsExpr(rhs).splice))
    }

    lazy val assignToJsBinOpExpr: ToExpr[JsBinOpExpr] = {
      case Assignment(op, lhs, rhs) =>
        reify(JsBinOpExpr(c.literal(op).splice, toJsExpr(lhs).splice, toJsExpr(rhs).splice))
    }

    lazy val toJsCallExpr: ToExpr[JsCallExpr] = {
      case Apply(fun, args) =>
        val callee = toJsExpr(fun)
        val params = listOfExprToExprOfList(args.map(toJsExpr))
        reify(JsCallExpr(callee.splice, params.splice))
     }

    lazy val toJsBlockStmt: ToExpr[JsBlockStmt] = {
      case Block(init, last) =>
        val stmtTrees = if (isUnitLiteral(last)) init else init :+ last
        val stmts = listOfExprToExprOfList(stmtTrees map toJsStmt)
        reify(JsBlockStmt(stmts.splice))
    }

    lazy val toJsExprStmt: ToExpr[JsExprStmt] = toJsExpr andThen (jsExpr => reify(JsExprStmt(jsExpr.splice)))

    lazy val toToJsIfStmt: ToExpr[JsIfStmt] = {
      case If(cond, thenp, elsep) =>
        val condJsExpr = toJsExpr(cond)
        val thenJsExpr = toJsStmt(thenp)
        val elseJsStmt = if (isUnitLiteral(elsep)) reify(None) else reify(Some(toJsStmt(elsep).splice))
        reify(JsIfStmt(condJsExpr.splice, thenJsExpr.splice, elseJsStmt.splice))
    }

    lazy val toJsWhileStmt: ToExpr[JsWhileStmt] = {
      case LabelDef(termName, Nil, If(cond, Block(List(body), _), _)) if termName.encoded.startsWith("while$") =>
        val condJsExpr = toJsExpr(cond)
        val bodyJsStmt = if (isUnitLiteral(body)) reify(JsEmptyStmt) else toJsStmt(body)
        reify(JsWhileStmt(condJsExpr.splice, bodyJsStmt.splice))
    }

    lazy val toJsVarDefStmt: ToExpr[JsVarDefStmt] = {
      case ValDef(mods, name, _, rhs) =>
        val identifier = c.literal(name.encoded)
        val initializer = toJsExpr(rhs)
        val isConst = c.literal(!mods.hasFlag(Flag.MUTABLE))
        reify(JsVarDefStmt(identifier.splice, initializer.splice, isConst.splice))
    }

    lazy val toJsFunDeclStmt: ToExpr[JsFunDeclStmt] = {
      case DefDef(_, name, _, List(vparams), _, rhs) =>
        val identifier = c.literal(name.encoded)
        val params = listOfExprToExprOfList(vparams.map(v => c.literal(v.name.encoded)))
        val body = if (isUnitLiteral(rhs)) reify(JsEmptyStmt) else toJsStmt(rhs)
        reify(JsFunDeclStmt(identifier.splice, params.splice, body.splice))
    }

    lazy val toJsExpr: ToExpr[JsExpr] = Seq(
      toJsBooleanLiteral,
      toJsStringLiteral,
      intToJsStringLiteral,
      doubleToJsStringLiteral,
      toJsIdentifierExpr,
      toJsUnOpExpr,
      toJsBinOpExpr,
      assignToJsBinOpExpr,
      toJsCallExpr
    ) reduceLeft (_ orElse _)

    lazy val toJsStmt: ToExpr[JsStmt] = Seq(
      toJsBlockStmt,
      toJsExprStmt,
      toToJsIfStmt,
      toJsWhileStmt,
      toJsVarDefStmt,
      toJsFunDeclStmt
    ) reduceLeft (_ orElse _)

    val toJsAst: PartialFunction[Tree, Expr[JsAst]] = toJsExpr orElse toJsStmt

    toJsAst(expr.tree)
  }
}
