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
      val expressions = value.asJsObject.fields.map { case (variable, exprValue) =>
        exprValue match {
          case JsString(jsString) =>
            SimpleExpression(variable, jsString)
          case JsNumber(jsNumber) =>
            SimpleExpression(variable, jsNumber)
          case JsBoolean(jsBool) =>
            SimpleExpression(variable, jsBool)
          case JsObject(expr) =>
            ComplexExpression(variable, expr)
          case _ =>
            throw new UnsupportedOperationException("not supported.")
        }
      }.toList

      AndBlock(expressions)
    }

    def write(andBlock: AndBlock) = ???
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
  case class AndBlock(expressions: List[Expression])

  abstract class Expression(variable: String, value: Any)
  case class ComplexExpression(variable: String, expression: Map[String, JsValue]) extends Expression(variable, expression)
  case class SimpleExpression(variable: String, value: Any) extends Expression(variable, value)

}
