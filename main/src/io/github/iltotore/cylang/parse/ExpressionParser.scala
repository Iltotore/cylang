package io.github.iltotore.cylang.parse

import io.github.iltotore.cylang.{CYType, Parameter, execute}
import io.github.iltotore.cylang.ast.Expression.*
import io.github.iltotore.cylang.ast.{Body, Expression, Structure, Value}

import scala.util.matching.Regex
import scala.util.parsing.combinator.*
import scala.util.parsing.input.Position

object ExpressionParser extends CYParsers {
  
  val word: Parser[String] = raw"\w+".r

  def program: Parser[ProgramDeclaration] = "PROGRAMME" ~> word ~ rep(not(body) ~> declaration) ~ body mapWithPos {
    case name ~ declarations ~ main => ProgramDeclaration(name, declarations, main)
  }

  def expression: Parser[Expression] = variableAssignment

  def empty: Parser[Empty] = successWithPos(Empty())

  //Literal
  def bool: Parser[Literal] = raw"(true)|(false)".r mapWithPos { x => Literal(Value.Bool(x.toBoolean)) }

  def text: Parser[Literal] = "\\\"[^\\\"]*\\\"".r mapWithPos { x => Literal(Value.Text(x.substring(1, x.length - 1))) }

  def character: Parser[Literal] = raw"'.'".r mapWithPos { x => Literal(Value.Character(x.charAt(1))) }

  def real: Parser[Literal] = raw"[0-9]+\.[0-9]+".r mapWithPos { x => Literal(Value.Real(x.toDouble)) }

  def integer: Parser[Literal] = raw"[0-9]+".r mapWithPos { x => Literal(Value.Integer(x.toInt)) }

  def literalSymbol: Parser[Literal] = bool | text | character | real | integer

  //Misc
  def paranthesized: Parser[Expression] = "(" ~> expression <~ ")"

  def variableCall: Parser[VariableCall] = word mapWithPos VariableCall.apply

  def functionCall: Parser[FunctionCall] = word ~ ("(" ~> repsep(expression, ",") <~ ")") mapWithPos {
    case name ~ args => FunctionCall(name, args)
  }

  def rawType: Parser[CYType] = word map (
    name => CYType
      .rawTypes
      .find(_.name equals name)
      .getOrElse(CYType.StructureInstance(name))
    )

  def arrayType: Parser[CYType] = "tableau de type" ~> cyType ~ ("de taille" ~> raw"[0-9]+".r).? ^^ {
    case tpe ~ size => CYType.Array(tpe, size.map(_.toInt))
  }

  def cyType: Parser[CYType] = arrayType | rawType

  def param: Parser[Parameter] = word ~ ":" ~ cyType ^^ { case name ~ _ ~ tpe => Parameter(name, tpe) }

  def body: Parser[Body] = ("VARIABLE" ~> rep(not("DEBUT") ~> param)).? ~ "DEBUT" ~ tree("FIN") ^^ {
    case Some(variables) ~ _ ~ expr => Body(variables, expr)
    case None ~ _ ~ expr => Body(List.empty, expr)
  }

  def constantDeclaration = "CONSTANTE" ~> raw"\w+".r ~ "<-" ~ expression mapWithPos { case name ~ _ ~ expr => ConstantDeclaration(name, expr) }

  def enumerationDeclaration: Parser[EnumerationDeclaration] = "ENUMERATION" ~> word ~ rep(not("FIN ENUMERATION") ~> ",".? ~> word) <~ "FIN ENUMERATION" mapWithPos {
    case name ~ fields => EnumerationDeclaration(name, fields)
  }

  def structureDeclaration: Parser[StructureDeclaration] = "STRUCTURE" ~> word ~ rep(not("FIN STRUCTURE") ~> param) <~ "FIN STRUCTURE" mapWithPos {
    case name ~ fields => StructureDeclaration(name, fields)
  }

  def functionDeclaration: Parser[FunctionDeclaration] = "FONCTION" ~> word ~ ("(" ~> repsep(param, ",") <~ ")") ~ ":" ~ cyType ~ body mapWithPos {
    case name ~ params ~ _ ~ tpe ~ b => FunctionDeclaration(name, tpe, params, b)
  }

  def procedureDeclaration: Parser[FunctionDeclaration] = "PROCEDURE" ~> word ~ ("(" ~> repsep(param, ",") <~ ")") ~ body mapWithPos {
    case name ~ params ~ b => FunctionDeclaration(name, CYType.Void, params, b)
  }

  def declaration: Parser[Expression] = constantDeclaration | enumerationDeclaration | structureDeclaration | functionDeclaration | procedureDeclaration

  private val binaryOps: Map[String, Position ?=> (Expression, Expression) => Expression] = Map(
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
  def forLoop: Parser[ForLoop] = "POUR" ~> word ~ "DE" ~ expression ~ "A" ~ expression ~ ("PAS DE" ~> expression).? ~ "FAIRE" ~ tree("FIN POUR") mapWithPos {
    case param ~ _ ~ from ~ _ ~ to ~ Some(step) ~ _ ~ expr => ForLoop(param, from, to, step, expr)
    case param ~ _ ~ from ~ _ ~ to ~ None ~ _ ~ expr => ForLoop(param, from, to, Literal(Value.Integer(1)), expr)
  }

  def whileLoop: Parser[WhileLoop] = "TANT QUE" ~> expression ~ "FAIRE" ~ tree("FIN TANT QUE") mapWithPos { case cond ~ _ ~ expr => WhileLoop(cond, expr) }

  def doWhileLoop: Parser[DoWhileLoop] = "FAIRE" ~> tree("TANT QUE") ~ expression mapWithPos { case expr ~ cond => DoWhileLoop(cond, expr)}
  
  def ifElse: Parser[If] = "SI" ~> expression ~ "ALORS" ~ ifBody mapWithPos {
    case cond ~ _ ~ (expr ~ elseExpr) => If(cond, expr, elseExpr)
  }

  def ifBody: Parser[~[Expression, Expression]] = (tree("SINON") ~ (ifElse | tree("FIN SI"))) | (tree("FIN SI") ~ empty)

  def treeReturn: Parser[Expression] = "RETOURNER" ~> expression mapWithPos Return.apply

  def treeInvocable: Parser[Expression] = forLoop | whileLoop | doWhileLoop | ifElse | treeReturn

  def tree(end: Parser[?]) = rep(not(end) ~> (treeInvocable | expression)) <~ end mapWithPos Tree.apply

  def variableAssignment: Parser[Expression] = ((word ~ "<-" ~ equality) mapWithPos { case name ~ _ ~ expr => VariableAssignment(name, expr) }) | arrayAssignment

  def arrayAssignment: Parser[Expression] = ((invocable ~ ("[" ~> equality <~ "]") ~ "<-" ~ equality) mapWithPos { case array ~ index ~ _ ~ expr => ArrayAssignment(array, index, expr)}) | structureAssignment

  def structureAssignment: Parser[Expression] = ((invocable ~ ("." ~> word) ~ "<-" ~ equality) mapWithPos { case structure ~ field ~ _ ~ expr => StructureAssignment(structure, field, expr) }) | equality

  def equality: Parser[Expression] = inequality * ("!?=".r mapWithPos binaryOps.apply)

  def inequality: Parser[Expression] = arith * ("[<>]=?".r mapWithPos binaryOps.apply)

  def arith: Parser[Expression] = term * ("[+-]".r mapWithPos binaryOps.apply)

  def term: Parser[Expression] = unary * ("[*/%]|(DIV)".r mapWithPos binaryOps.apply)

  private val unaryOps: Map[String, Position ?=> Expression => Expression] = Map(
    "+" -> (x => x),
    "-" -> Negation.apply,
    "!" -> Not.apply
  )

  def unary: Parser[Expression] = raw"[+\-!]".r.? ~ (invocable >> (left => furtherCall(left) | success(left))) mapWithPos {
    case Some(op) ~ expr => unaryOps(op)(expr)

    case None ~ expr => expr
  }

  def furtherCall(expr: Expression): Parser[Expression] = (arrayCall(expr) | structureCall(expr)) >> (left => furtherCall(left) | success(left))

  def arrayCall(expr: Expression): Parser[Expression] = ("[" ~> expression <~ "]") mapWithPos (ArrayCall(expr, _))

  def structureCall(expr: Expression): Parser[Expression] = ("." ~> word) mapWithPos (StructureCall(expr, _))

  def invocable: Parser[Expression] = literalSymbol | paranthesized | functionCall | variableCall

}