import spray.json._


object JsonSearchQueryProtocolTest extends JsonSearchQueryProtocol {
  def main(args: Array[String]) = {
    val queryJson = sampleJson.parseJson
    val searchQuery = queryJson.convertTo[SearchQuery]

    println(searchQuery)
  }
}

trait JsonSearchQueryProtocol extends DefaultJsonProtocol {

  implicit val queryFormat: JsonFormat[Query] = jsonFormat1(Query)
  implicit val searchQueryFormat: JsonFormat[SearchQuery] = jsonFormat3(SearchQuery)

  implicit object AndBlockJsonFormat extends RootJsonFormat[AndBlock] {
    def read(value: JsValue) = {
      // TODO: improve this code.
      val expressions = value.asJsObject.fields.map { case (variable, exprValue) =>
        exprValue match {
          case JsString(jsString) =>
            SimpleExpr(variable, jsString)
          case JsNumber(jsNumber) =>
            SimpleExpr(variable, jsNumber)
          case JsBoolean(jsBool) =>
            SimpleExpr(variable, jsBool)
          case JsObject(expr) =>
            val exprValues: List[ExprValue] = expr.map { case (operation, exprValue) =>
                if (operation == "in") {
                  exprValue match {
                    case JsArray(elements) =>
                      InExpr(elements.toList)
                    case _ =>
                      throw new UnsupportedOperationException("'in' operation supports only array-typed value")
                  }
                } else if (operation == "ne") {
                  exprValue match {
                    case JsString(jsString) => NeExpr(jsString)
                    case JsNumber(jsNumber) => NeExpr(jsNumber)
                    case _ =>
                      throw new UnsupportedOperationException("'ne' operation supports only number and string value")
                  }
                } else if (operation == "gte") {
                  GteExpr(exprValue)
                } else if (operation == "lte") {
                  LteExpr(exprValue)
                } else if (operation == "lt") {
                  LtExpr(exprValue)
                } else if (operation == "gt") {
                  GtExpr(exprValue)
                } else {
                  throw new UnsupportedOperationException(s"operation $operation is not supported")
                }
            }.toList

            ComplexExpr(variable, exprValues)
          case _ =>
            throw new UnsupportedOperationException("not supported.")
        }
      }.toList

      AndBlock(expressions)
    }

    def write(andBlock: AndBlock) = throw new UnsupportedOperationException("conversion to json is not supported")
  }

  def sampleJson =
    """
      |{
      |	"query": {
      |		"and": {
      |			"bc": 1234,
      |			"stamm": 852133,
      |			"freetext": "he said \"hi\" what the fuck ",
      |			"birthday": {
      |				"gte": "2011-07-14T00:00:0+0000",
      |				"lte": "2011-07-14T24:00:0+0000"
      |			},
      |			"age": {
      |				"gte": 1,
      |				"lte": 99
      |			},
      |			"name": {
      |				"ne": "allen"
      |			},
      |			"tags": {
      |				"in": ["hello", 123]
      |			}
      |		}
      |	}
      |}
    """.stripMargin


  case class SearchQuery(query: Query, count: Option[Int] = Some(50), offset: Option[Int] = Some(0))
  case class Query(and: AndBlock)
  case class AndBlock(expressions: List[Expr])

  abstract class Expr(variable: String, value: Any)
  case class ComplexExpr(variable: String, expression: List[ExprValue]) extends Expr(variable, expression)
  case class SimpleExpr(variable: String, value: Any) extends Expr(variable, value)

  abstract class ExprValue(operation: String, value: Any)
  case class InExpr(value: List[Any]) extends ExprValue("in", value)
  case class NeExpr(value: Any) extends ExprValue("ne", value)
  case class GteExpr(value: Any) extends ExprValue("gte", value)
  case class LteExpr(value: Any) extends ExprValue("lte", value)
  case class LtExpr(value: Any) extends ExprValue("lt", value)
  case class GtExpr(value: Any) extends ExprValue("gt", value)
}
