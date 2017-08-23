import scala.util.parsing.combinator.JavaTokenParsers

object SearchQueryDSLParser extends SearchQueryDSL {
  def main(args: Array[String]): Unit = {
//    println(parseAll(fullExpr, "namespace_eq_test_and_schema_neq_\"\\\"gavno\\\"\""))
    println(parseAll(fullExpr, "namespace_eq_test"))
    println(parseAll(fullExpr, "namespace_eq_test_and_schema_neq_xsd"))
    println(parseAll(fullExpr, "value"))
    println(parseAll(fullExpr, "\"armaggedon\""))
  }
}

trait SearchQueryDSL extends JavaTokenParsers {
  def unquotedString: Parser[String] = "[a-zA-Z0-9]+".r
  def quotedString: Parser[String] = stringLiteral
  def operator: Parser[ExprOperator] = ("_eq_" | "_neq_" | "_lt_" | "_gt_" | "_lte_" | "_gte_") ^^ { op =>
    if (op == "_eq_") EqOperator()
    else if (op == "_neq_") NeqOperator()
    else if (op == "_lt_") LessOperator()
    else if (op == "_gt_") GreaterOperator()
    else if (op == "_lte_") LteOperator()
    else if (op == "_gte_") GteOperator()
    else throw new IllegalArgumentException(s"Unknown operation $op")
  }
  def separator: Parser[AndOperator] = "_and_" ^^ { _ => AndOperator() }

  def valueExpr: Parser[ValueBlock] = (unquotedString | quotedString) ^^ {
    case (value) => ValueBlock(value)
  }
  def complexExpr: Parser[ExprBlock] = unquotedString ~ operator ~ (unquotedString | quotedString)^^ {
    case (variable ~ op ~ value) => ExprBlock(variable, op, value)
  }
  def expr: Parser[Block] = complexExpr | valueExpr
  def fullExpr: Parser[List[Block]] = repsep(expr, separator)
}

abstract class Block
case class ValueBlock(value: String) extends Block {
  override def toString: String = s"($value)"
}
case class ExprBlock(leftExpr: String, operator: ExprOperator, rightExpr: String) extends Block {
  override def toString: String = {
    operator match {
      case EqOperator() =>
        s"+($leftExpr:$rightExpr)"
      case NeqOperator() =>
        s"-($leftExpr:$rightExpr)"
      case _ =>
        // TODO: implement new operations for search
        ???
    }
  }
}

sealed abstract class Operator(raw: String)
abstract class ExprOperator(raw: String) extends Operator(raw)
abstract class NonExprOperator(raw: String) extends Operator(raw)
case class AndOperator() extends NonExprOperator("_and_")
case class EqOperator() extends ExprOperator("_eq_")
case class NeqOperator() extends ExprOperator("_neq_")
case class LteOperator() extends ExprOperator("_leq_")
case class GteOperator() extends ExprOperator("_gte_")
case class LessOperator() extends ExprOperator("_le_")
case class GreaterOperator() extends ExprOperator("_gt_")