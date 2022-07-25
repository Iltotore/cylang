package io.github.iltotore.cylang.parse

import io.github.iltotore.cylang.{CYType, Parameter, execute}
import io.github.iltotore.cylang.ast.Expression.*
import io.github.iltotore.cylang.ast.{Body, Expression, Value}
import Token.*

import scala.util.matching.Regex
import scala.util.parsing.combinator.*
import scala.util.parsing.input.Position


object ExpressionParser extends CYParsers with FrenchParser {

  override type Elem = Token

  //Tokens with parameters
  private def literalBool: Parser[LiteralBool] = accept("boolean literal", { case x: LiteralBool => x })
  private def literalInt: Parser[LiteralInt] = accept("integer literal", { case x: LiteralInt => x })
  private def literalReal: Parser[LiteralReal] = accept("real literal", { case x: LiteralReal => x })
  private def literalChar: Parser[LiteralChar] = accept("char literal", { case x: LiteralChar => x })
  private def literalText: Parser[LiteralText] = accept("text literal", { case x: LiteralText => x })
  private def identifier: Parser[Identifier] = accept("identifier", { case x: Identifier => x })
  private def operator: Parser[Operator] = accept("operator", { case x: Operator => x })

  def program: Parser[ProgramDeclaration] = Program() ~> identifier ~ rep(not(body) ~> declaration) ~ body mapWithPos {
    case Identifier(name) ~ declarations ~ main => ProgramDeclaration(name, declarations, main)
  }

  def expression: Parser[Expression] = variableAssignment

  def empty: Parser[Empty] = successWithPos(Empty())

  //Literal
  def bool: Parser[Literal] = literalBool mapWithPos { case LiteralBool(value) => Literal(Value.Bool(value)) }
  def integer: Parser[Literal] = literalInt mapWithPos { case LiteralInt(value) => Literal(Value.Integer(value)) }
  def real: Parser[Literal] = literalReal mapWithPos { case LiteralReal(value) => Literal(Value.Real(value)) }
  def character: Parser[Literal] = literalChar mapWithPos { case LiteralChar(value) => Literal(Value.Character(value)) }
  def text: Parser[Literal] = literalText mapWithPos { case LiteralText(value) => Literal(Value.Text(value)) }

  def literalSymbol: Parser[Literal] = bool | text | character | real | integer

  //Misc
  def parenthesized: Parser[Expression] = ParenthesisOpen() ~>! expression <~! ParenthesisClose()

  def variableCall: Parser[VariableCall] = identifier mapWithPos { case Identifier(name) => VariableCall(name) }

  def functionCall: Parser[FunctionCall] = identifier ~ (ParenthesisOpen() ~>! repsep(expression, Comma()) <~! ParenthesisClose()) mapWithPos {
    case Identifier(name) ~ args => FunctionCall(name, args)
  }

  def rawType: Parser[CYType] = identifier map {
    case Identifier(name) => CYType
      .rawTypes
      .find(_.name equals name)
      .getOrElse(CYType.StructureInstance(name))
  }

  def arrayType: Parser[CYType] = ArrayOf() ~>! cyType ~! (ArraySize() ~> literalInt).? ^^ {
    case tpe ~ size => CYType.Array(tpe, size.map(_.value))
  }

  def cyType: Parser[CYType] = arrayType | rawType

  def param: Parser[Parameter] = identifier ~ Colon() ~! cyType ^^ { case Identifier(name) ~ _ ~ tpe => Parameter(name, tpe) }

  def body: Parser[Body] = (Variable() ~>! rep(not(Begin()) ~> param)).? ~! Begin() ~! tree(End()) ^^ {
    case Some(variables) ~ _ ~ expr => Body(variables, expr)
    case None ~ _ ~ expr => Body(List.empty, expr)
  }

  def constantDeclaration: Parser[ConstantDeclaration] = Constant() ~>! identifier ~! Assignment() ~! expression mapWithPos { case Identifier(name) ~ _ ~ expr => ConstantDeclaration(name, expr) }

  def enumerationDeclaration: Parser[EnumerationDeclaration] = Enumeration() ~>! identifier ~! rep(not(End() ~! Enumeration()) ~>! Comma().? ~>! identifier) <~ (End() ~ Enumeration()) mapWithPos {
    case Identifier(name) ~ fields => EnumerationDeclaration(name, fields.map(_.value))
  }

  def structureDeclaration: Parser[StructureDeclaration] = Structure() ~>! identifier ~! rep(not(End() ~! Structure()) ~>! param) <~ (End() ~! Structure()) mapWithPos {
    case Identifier(name) ~ fields => StructureDeclaration(name, fields)
  }

  def functionDeclaration: Parser[FunctionDeclaration] = Function() ~>! identifier ~! (ParenthesisOpen() ~>! repsep(param, Comma()) <~! ParenthesisClose()) ~! Colon() ~! cyType ~! body mapWithPos {
    case Identifier(name) ~ params ~ _ ~ tpe ~ b => FunctionDeclaration(name, tpe, params, b)
  }

  def procedureDeclaration: Parser[FunctionDeclaration] = Procedure() ~> identifier ~ (ParenthesisOpen() ~> repsep(param, Comma()) <~ ParenthesisClose()) ~ body mapWithPos {
    case Identifier(name) ~ params ~ b => FunctionDeclaration(name, CYType.Void, params, b)
  }

  def declaration: Parser[Expression] = constantDeclaration | enumerationDeclaration | structureDeclaration | functionDeclaration | procedureDeclaration

  //Binary Operators
  //POUR i DE 0 A 10 FAIRE
  def forLoop: Parser[ForLoop] = For() ~> identifier ~ From() ~ expression ~ To() ~ expression ~ (Step() ~> expression).? ~ Do() ~ tree(End() ~ For()) mapWithPos {
    case Identifier(param) ~ _ ~ from ~ _ ~ to ~ Some(step) ~ _ ~ expr => ForLoop(param, from, to, step, expr)
    case Identifier(param) ~ _ ~ from ~ _ ~ to ~ None ~ _ ~ expr => ForLoop(param, from, to, Literal(Value.Integer(1)), expr)
  }

  def whileLoop: Parser[WhileLoop] = While() ~> expression ~ Do() ~ tree(End() ~ While()) mapWithPos { case cond ~ _ ~ expr => WhileLoop(cond, expr) }

  def doWhileLoop: Parser[DoWhileLoop] = Do() ~> tree(While()) ~ expression mapWithPos { case expr ~ cond => DoWhileLoop(cond, expr)}

  def ifElse: Parser[IfCondition] = If() ~> expression ~ Then() ~ ifBody mapWithPos {
    case cond ~ _ ~ (expr ~ elseExpr) => IfCondition(cond, expr, elseExpr)
  }

  def ifBody: Parser[~[Expression, Expression]] = (tree(Else(), hard=false) ~ (ifElse | tree(End() ~ If(), hard=false))) | (tree(End() ~ If()) ~ empty)

  def treeReturn: Parser[Expression] = Return() ~> (expression | empty) mapWithPos ReturnExpr.apply

  def treeInvocable: Parser[Expression] = forLoop | whileLoop | doWhileLoop | ifElse | treeReturn

  def tree(end: Parser[?], hard: Boolean = true): Parser[Tree] =
    if(hard) rep(not(end) ~> (treeInvocable | expression)) <~! end mapWithPos Tree.apply
    else rep(not(end) ~> (treeInvocable | expression)) <~ end mapWithPos Tree.apply

  def variableAssignment: Parser[Expression] = ((identifier ~ Assignment() ~ comparison) mapWithPos { case Identifier(name) ~ _ ~ expr => VariableAssignment(name, expr) }) | arrayAssignment

  def arrayAssignment: Parser[Expression] = ((invocable ~ (BracketOpen() ~> comparison <~ BracketClose()) ~ Assignment() ~ comparison) mapWithPos { case array ~ index ~ _ ~ expr => ArrayAssignment(array, index, expr)}) | structureAssignment

  def structureAssignment: Parser[Expression] = ((invocable ~ (Dot() ~> identifier) ~ Assignment() ~ comparison) mapWithPos { case structure ~ Identifier(field) ~ _ ~ expr => StructureAssignment(structure, field, expr) }) | comparison

  private val compOps: Map[String, Position ?=> (Expression, Expression) => Expression] = Map(
    "=" -> Equality.apply,
    "!=" -> ((a, b) => Not(Equality(a, b))),
    ">" -> Greater.apply,
    ">=" -> GreaterEqual.apply,
    "<" -> Less.apply,
    "<=" -> LessEqual.apply
  )

  private val arithOps: Map[String, Position ?=> (Expression, Expression) => Expression] = Map(
    "+" -> Addition.apply,
    "-" -> Subtraction.apply
  )

  private val termOps: Map[String, Position ?=> (Expression, Expression) => Expression] = Map(
    "*" -> Multiplication.apply,
    "/" -> Division.apply,
    "DIV" -> WholeDivision.apply,
    "MOD" -> Modulo.apply
  )

  private val unaryOps: Map[String, Position ?=> Expression => Expression] = Map(
    "+" -> (x => x),
    "-" -> Negation.apply,
    "!" -> Not.apply
  )

  def comparison: Parser[Expression] = arith * (operator partialMapWithPos { case Operator(op) if compOps.contains(op) => compOps(op) })

  def arith: Parser[Expression] = term * (operator partialMapWithPos { case Operator(op) if arithOps.contains(op) => arithOps(op) })

  def term: Parser[Expression] = unary * (operator partialMapWithPos { case Operator(op) if termOps.contains(op) => termOps(op) })

  def unary: Parser[Expression] = operator.? ~ (invocable >> (left => furtherCall(left) | success(left))) partialMapWithPos {
    case Some(Operator(op)) ~ expr if unaryOps.contains(op) => unaryOps(op)(expr)

    case None ~ expr => expr
  }

  def furtherCall(expr: Expression): Parser[Expression] = (arrayCall(expr) | structureCall(expr)) >> (left => furtherCall(left) | success(left))

  def arrayCall(expr: Expression): Parser[Expression] = (BracketOpen() ~> expression <~ BracketClose()) mapWithPos (ArrayCall(expr, _))

  def structureCall(expr: Expression): Parser[Expression] = (Dot() ~> identifier) mapWithPos { case Identifier(name) => StructureCall(expr, name) }

  def invocable: Parser[Expression] = literalSymbol | parenthesized | functionCall | variableCall

  def apply(tokens: List[Token]): Either[ParsingError, Expression] = program(TokenReader(tokens)) match {
    case Success(result, in) if in.atEnd => Right(result)
    case Success(_, _) => Left(ParsingError(s"Le programme doit se terminer par `FIN`"))
    case NoSuccess(msg, next) => Left(ParsingError(msg, next.pos))
  }
}
