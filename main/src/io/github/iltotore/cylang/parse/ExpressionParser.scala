package io.github.iltotore.cylang.parse

import io.github.iltotore.cylang.{CYType, Parameter, execute}
import io.github.iltotore.cylang.ast.Expression.*
import io.github.iltotore.cylang.ast.{Expression, Structure, Value}

import scala.util.parsing.combinator.*

object ExpressionParser extends RegexParsers with CYParsers {

  def program: Parser[ProgramDeclaration] = "PROGRAMME" ~> raw"\w+".r ~ rep(not(body) ~> declaration) ~ body ^^ {
    case name ~ declarations ~ main => ProgramDeclaration(name, declarations, main)
  }

  def expression: Parser[Expression] = variableAssignment

  def empty = success(Empty)

  //Literal
  def bool: Parser[Literal] = raw"(true)|(false)".r ^^ { x => Literal(Value.Bool(x.toBoolean)) }

  def text: Parser[Literal] = "\\\".*\\\"".r ^^ { x => Literal(Value.Text(x.substring(1, x.length - 1))) }

  def character: Parser[Literal] = raw"'.'".r ^^ { x => Literal(Value.Character(x.charAt(1))) }

  def real: Parser[Literal] = raw"[0-9]+\.[0-9]+".r ^^ { x => Literal(Value.Real(x.toDouble)) }

  def integer: Parser[Literal] = raw"[0-9]+".r ^^ { x => Literal(Value.Integer(x.toInt)) }

  def literalSymbol = bool | text | character | real | integer

  //Misc
  def paranthesized = "(" ~> expression <~ ")"

  def variableCall = raw"\w+".r ^^ VariableCall.apply

  def functionCall = raw"\w+".r ~ ("(" ~> repsep(expression, ",") <~ ")") ^^ {
    case name ~ args => FunctionCall(name, args)
  }

  def rawType = raw"\w+".r map (
    name => CYType
      .rawTypes
      .find(_.name equals name)
      .getOrElse(CYType.StructureInstance(name))
    )

  def arrayType = "tableau de type" ~> cyType ~ ("de taille" ~> raw"[0-9]+".r).? ^^ {
    case tpe ~ size => CYType.Array(tpe, size.map(_.toInt))
  }

  def cyType: Parser[CYType] = arrayType | rawType

  def param = raw"\w+".r ~ ":" ~ cyType ^^ { case name ~ ":" ~ tpe => Parameter(name, tpe) }

  def body = ("VARIABLE" ~> rep(not("DEBUT") ~> param)).? ~ "DEBUT" ~ tree("FIN") ^^ {
    case Some(variables) ~ "DEBUT" ~ expr => Body(variables, expr)
    case None ~ "DEBUT" ~ expr => Body(List.empty, expr)
  }

  def enumerationDeclaration = "ENUMERATION" ~> raw"\w+".r ~ rep(not("FIN ENUMERATION") ~> ",".? ~> raw"\w+".r) <~ "FIN ENUMERATION" ^^ {
    case name ~ fields => EnumerationDeclaration(name, fields)
  }

  def structureDeclaration = "STRUCTURE" ~> raw"\w+".r ~ rep(not("FIN STRUCTURE") ~> param) <~ "FIN STRUCTURE" ^^ {
    case name ~ fields => StructureDeclaration(name, fields)
  }

  def functionDeclaration = "FONCTION" ~> raw"\w+".r ~ ("(" ~> repsep(param, ",") <~ ")") ~ ":" ~ cyType ~ body ^^ {
    case name ~ params ~ ":" ~ tpe ~ b => FunctionDeclaration(name, tpe, params, b)
  }

  def declaration = enumerationDeclaration | structureDeclaration | functionDeclaration

  private val binaryOps: Map[String, (Expression, Expression) => Expression] = Map(
    "=" -> Equality.apply,
    ">" -> Greater.apply,
    ">=" -> GreaterEqual.apply,
    "<" -> Less.apply,
    "<=" -> LessEqual.apply,
    "+" -> Addition.apply,
    "-" -> Subtraction.apply,
    "*" -> Multiplication.apply,
    "/" -> Division.apply,
    "DIV" -> WholeDivision.apply,
    "%" -> Modulo.apply
  )

  //Binary Operators
  //POUR i DE 0 A 10 FAIRE
  def forLoop: Parser[ForLoop] = "POUR" ~> raw"\w+".r ~ "DE" ~ expression ~ "A" ~ expression ~ ("PAS DE" ~> expression).? ~ "FAIRE" ~ tree("FIN POUR") ^^ {
    case param ~ "DE" ~ from ~ "A" ~ to ~ Some(step) ~ "FAIRE" ~ expr => ForLoop(param, from, to, step, expr)
    case param ~ "DE" ~ from ~ "A" ~ to ~ None ~ "FAIRE" ~ expr => ForLoop(param, from, to, Literal(Value.Integer(1)), expr)
  }

  def whileLoop: Parser[WhileLoop] = "TANT QUE" ~> expression ~ "FAIRE" ~ tree("FIN TANT QUE") ^^ { case cond ~ "FAIRE" ~ expr => WhileLoop(cond, expr) }

  def ifElse: Parser[If] = "SI" ~> expression ~ "FAIRE" ~ ifBody ^^ {
    case cond ~ "FAIRE" ~ (expr ~ elseExpr) => If(cond, expr, elseExpr)
  }

  def ifBody: Parser[~[Expression, Expression]] = (tree("SINON") ~ (ifElse | ("FAIRE" ~> tree("FIN SI")))) | (tree("FIN SI") ~ empty)

  def treeReturn = "RETOURNER" ~> expression ^^ Return.apply

  def treeInvocable = forLoop | whileLoop | ifElse | treeReturn

  def tree(end: Parser[?]) = rep(not(end) ~> (treeInvocable | expression)) <~ end ^^ Tree.apply

  def variableAssignment = (raw"\w+".r ~ "<-" ~ equality ^^ { case name ~ _ ~ expr => VariableAssignment(name, expr) }) | arrayAssignment

  def arrayAssignment = invocable ~ ("[" ~> equality <~ "]") ~ "<-" ~ equality ^^ { case array ~ index ~ _ ~ expr => ArrayAssignment(array, index, expr)} | structureAssignment

  def structureAssignment = invocable ~ ("." ~> raw"\w+".r) ~ "<-" ~ equality ^^ { case structure ~ field ~ _ ~ expr => StructureAssignment(structure, field, expr) } | equality

  def equality = inequality * ("!?=".r ^^ binaryOps.apply)

  def inequality = arith * ("[<>]=?".r ^^ binaryOps.apply)

  def arith = term * ("[+-]".r ^^ binaryOps.apply)

  def term = unary * ("[*/%]|(DIV)".r ^^ binaryOps.apply)

  private val unaryOps: Map[String, Expression => Expression] = Map(
    "+" -> (x => x),
    "-" -> Negation.apply,
    "!" -> Not.apply
  )

  def unary = raw"[+\-!]".r.? ~ (invocable >> (left => furtherCall(left) | success(left))) ^^ {
    case Some(op) ~ expr => unaryOps(op)(expr)

    case None ~ expr => expr
  }

  def furtherCall(expr: Expression): Parser[Expression] = (arrayCall(expr) | structureCall(expr)) >> (left => furtherCall(left) | success(left))

  def arrayCall(expr: Expression): Parser[Expression] = ("[" ~> expression <~ "]") ^^ (ArrayCall(expr, _))

  def structureCall(expr: Expression): Parser[Expression] = ("." ~> raw"\w+".r) ^^ (StructureCall(expr, _))

  def invocable = literalSymbol | paranthesized | functionCall | variableCall

}