/*
Author: Angel G. Carrillo Laguna
Git: AngelGCL
CICOM Parser: Parses a string writteng in CICOM language style, provided by Prof. Wilson Rivera @ UPRM
 */
import scala.util.parsing.combinator._

object CICOMParser extends RegexParsers {
  def reserved = not("to"|"in"|"let"|"if"|"map"|"true"|"false"|"null")
  def number:CICOMParser.Parser[Any] = "[0-9_]".r ^^ { str => number}
  def Character:CICOMParser.Parser[Any] = "[a-zA-Z_][a-zA-Z_]*".r
  def Def :CICOMParser.Parser[Any]= (Id ~ ":=" ~ Exp.+ ~ ";")
  def Id:CICOMParser.Parser[Any] = reserved~Character~({reserved~Character|reserved~number}* )^^ { str => Id}
  def Int:CICOMParser.Parser[Any] = number.+
  def Binop:CICOMParser.Parser[Any] = Sign | operators
  def Sign:CICOMParser.Parser[Any] = ("+"|"-") ^^ { str => Sign}
  def Unop:CICOMParser.Parser[Any] = (Sign | "~") ^^ { str => Unop}
  def Bool:CICOMParser.Parser[Any] = ("true"|"false") ^^ { str => Bool}
  def Empty:CICOMParser.Parser[Any] = "null"
  def PropIdList:CICOMParser.Parser[Any] = (Id~","~PropIdList)|Id
  def IdList:CICOMParser.Parser[Any] = {PropIdList}.*
  def PropExpList:CICOMParser.Parser[Any] =(Exp~","~ PropExpList)|Exp
  def ExpList:CICOMParser.Parser[Any] ={ PropExpList}.*
  def Factor :CICOMParser.Parser[Any]={"("~ Exp ~ ")"}|Prim|Id
  def Term :CICOMParser.Parser[Any] = Unop~Term | Factor  ~ rep({"("~ExpList ~")"}) |Empty | Int | Bool
  def Exp :CICOMParser.Parser[Any] = Term ~ { Binop ~ Term }.* |("if" ~ Exp ~ "then" ~ Exp ~ "else" ~ Exp) |
    "let" ~ rep(Def) ~ "in" ~ Exp | "map" ~ IdList ~ "to" ~ Exp

  def operators = "<="|">="|"!="|"&"|"|"|"*"|"-"|"+"|"/"|"%"|"<"|">"|"="

  def Prim: CICOMParser.Parser[Any] = "number?"|"function?"|"list?"|"null?"|"cons?"|"cons"|"first"|"rest"|"arity"

}


object Tester {
  def main(args: Array[String]): Unit = {
    val result = CICOMParser.parseAll(CICOMParser.Exp, "let\n"+
        "\tf := map n " +"to"+" if n = 0 then 1 else n * f(n - 1);\n"+
        "in\n"+
        "\tlet\n"+
        "\t\tf := map n,m,k to if (n <= 0 & n >= 0)\n"+
        "\t\t\t| (n < 0 & n > 0 & n != 0) then number?\n"+
        "\t\t\t\telse m / f(k + 1);\n"+
        "in\n"+
        "\tlet x:=3;\n"+
        "\t\ty:=4;\n"+
        "\t\tz:=cons?(function?(x * ~y), cons(-arity(x)));\n"+
        "\tin\n"+
        "\t\tlet x:=3;\n"+
        "\t\t\ty:=4;\n"+
        "\t\t\tz:=g();\n"+
        "\t\tin\n"+
        "\t\t\t(g(x,y,z))(null?(true),list?(false),first(null))")
    println(result.toString)
  }
}