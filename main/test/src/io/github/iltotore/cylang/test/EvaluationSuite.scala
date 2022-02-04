package io.github.iltotore.cylang.test

import scala.collection.mutable
import utest.*
import io.github.iltotore.cylang.{CYType, Context, Parameter, Scope, Variable}
import io.github.iltotore.cylang.ast.*
import io.github.iltotore.cylang.ast.Expression.*
import io.github.iltotore.cylang.eval.{*, given}

object EvaluationSuite extends TestSuite {

  val tests: Tests = Tests {

    given Context = Context.empty

    test("literal") - assertMatch(Literal(Value.Bool(true)).evaluate) { case Right((_, Value.Bool(true))) => }

    test("negation") {

      test - assertMatch(
        Negation(
          Literal(Value.Integer(1))
        ).evaluate
      ) { case Right((_, Value.Integer(-1))) => }

      test - assertMatch(
        Negation(
          Literal(Value.Real(1.5))
        ).evaluate
      ) { case Right((_, Value.Real(-1.5))) => }
    }

    test("addition") {
      test("integer") - assertMatch(
        Addition(
          Literal(Value.Integer(5)),
          Literal(Value.Integer(3))
        ).evaluate
      ) { case Right((_, Value.Integer(8))) => }

      test("real") {
        test - assertMatch(
          Addition(
            Literal(Value.Real(1.5)),
            Literal(Value.Real(2.5))
          ).evaluate
        ) { case Right((_, Value.Real(4))) => }

        test - assertMatch(
          Addition(
            Literal(Value.Integer(1)),
            Literal(Value.Real(2.5))
          ).evaluate
        ) { case Right((_, Value.Real(3.5))) => }

        test - assertMatch(
          Addition(
            Literal(Value.Real(2.5)),
            Literal(Value.Integer(1))
          ).evaluate
        ) { case Right((_, Value.Real(3.5))) => }
      }

      test("text") {

        test - assertMatch(
          Addition(
            Literal(Value.Text("A")),
            Literal(Value.Text("A"))
          ).evaluate
        ) { case Right((_, Value.Text("AA"))) => }

        test - assertMatch(
          Addition(
            Literal(Value.Integer(1)),
            Literal(Value.Text("A"))
          ).evaluate
        ) { case Right((_, Value.Text("1A"))) => }

        test - assertMatch(
          Addition(
            Literal(Value.Text("A")),
            Literal(Value.Integer(1))
          ).evaluate
        ) { case Right((_, Value.Text("A1"))) => }
      }
    }

    test("substraction") {

      test("integer") - assertMatch(
        Subtraction(
          Literal(Value.Integer(5)),
          Literal(Value.Integer(3))
        ).evaluate
      ) { case Right((_, Value.Integer(2))) => }

      test("real") {
        test - assertMatch(
          Subtraction(
            Literal(Value.Real(1.5)),
            Literal(Value.Real(2.5))
          ).evaluate
        ) { case Right((_, Value.Real(-1))) => }

        test - assertMatch(
          Subtraction(
            Literal(Value.Integer(1)),
            Literal(Value.Real(2.5))
          ).evaluate
        ) { case Right((_, Value.Real(-1.5))) => }

        test - assertMatch(
          Subtraction(
            Literal(Value.Real(2.5)),
            Literal(Value.Integer(1))
          ).evaluate
        ) { case Right((_, Value.Real(1.5))) => }
      }
    }

    test("multiplication") {

      test("integer") - assertMatch(
        Multiplication(
          Literal(Value.Integer(5)),
          Literal(Value.Integer(3))
        ).evaluate
      ) { case Right((_, Value.Integer(15))) => }

      test("real") {
        test - assertMatch(
          Multiplication(
            Literal(Value.Real(2.5)),
            Literal(Value.Real(0.5))
          ).evaluate
        ) { case Right((_, Value.Real(1.25))) => }

        test - assertMatch(
          Multiplication(
            Literal(Value.Integer(1)),
            Literal(Value.Real(2.5))
          ).evaluate
        ) { case Right((_, Value.Real(2.5))) => }

        test - assertMatch(
          Multiplication(
            Literal(Value.Real(2.5)),
            Literal(Value.Integer(1))
          ).evaluate
        ) { case Right((_, Value.Real(2.5))) => }
      }
    }

    test("division") {

      test("real") {

        test - assertMatch(
          Division(
            Literal(Value.Integer(5)),
            Literal(Value.Integer(2))
          ).evaluate
        ) { case Right((_, Value.Real(2.5))) => }

        test - assertMatch(
          Division(
            Literal(Value.Real(2.5)),
            Literal(Value.Real(0.5))
          ).evaluate
        ) { case Right((_, Value.Real(5))) => }

        test - assertMatch(
          Division(
            Literal(Value.Integer(1)),
            Literal(Value.Real(0.5))
          ).evaluate
        ) { case Right((_, Value.Real(2))) => }

        test - assertMatch(
          Division(
            Literal(Value.Real(1.5)),
            Literal(Value.Integer(2))
          ).evaluate
        ) { case Right((_, Value.Real(0.75))) => }
      }

      test("zero") - assertMatch(Division(Literal(Value.Real(5)), Literal(Value.Real(0))).evaluate) { case Left(_) => }
    }

    test("wholeDivision") {

      test("real") {

        test - assertMatch(
          WholeDivision(
            Literal(Value.Integer(5)),
            Literal(Value.Integer(2))
          ).evaluate
        ) { case Right((_, Value.Integer(2))) => }

        test - assertMatch(
          WholeDivision(
            Literal(Value.Real(2.5)),
            Literal(Value.Real(0.5))
          ).evaluate
        ) { case Right((_, Value.Integer(5))) => }

        test - assertMatch(
          WholeDivision(
            Literal(Value.Integer(1)),
            Literal(Value.Real(0.5))
          ).evaluate
        ) { case Right((_, Value.Integer(2))) => }

        test - assertMatch(
          WholeDivision(
            Literal(Value.Real(1.5)),
            Literal(Value.Integer(2))
          ).evaluate
        ) { case Right((_, Value.Integer(0))) => }
      }

      test("zero") - assertMatch(WholeDivision(Literal(Value.Real(5)), Literal(Value.Real(0))).evaluate) { case Left(_) => }
    }

    test("modulo") {

      test("real") {

        test - assertMatch(
          Modulo(
            Literal(Value.Integer(5)),
            Literal(Value.Integer(2))
          ).evaluate
        ) { case Right((_, Value.Real(1))) => }

        test - assertMatch(
          Modulo(
            Literal(Value.Real(2.5)),
            Literal(Value.Real(0.5))
          ).evaluate
        ) { case Right((_, Value.Real(0))) => }

        test - assertMatch(
          Modulo(
            Literal(Value.Integer(1)),
            Literal(Value.Real(0.5))
          ).evaluate
        ) { case Right((_, Value.Real(0))) => }

        test - assertMatch(
          Modulo(
            Literal(Value.Real(1.5)),
            Literal(Value.Integer(2))
          ).evaluate
        ) { case Right((_, Value.Real(1.5))) => }
      }

      test("zero") - assertMatch(
        Modulo(
          Literal(Value.Real(5)), Literal(Value.Real(0))
        ).evaluate
      ) { case Left(_) => }
    }

    test("equality") {

      test("true") {
        test - assertMatch(
          Equality(
            Literal(Value.Integer(5)),
            Literal(Value.Integer(5))
          ).evaluate
        ) { case Right((_, Value.Bool(true))) => }

        test - assertMatch(
          Equality(
            Literal(Value.Integer(5)),
            Literal(Value.Real(5))
          ).evaluate
        ) { case Right((_, Value.Bool(true))) => }
      }

      test("false") {
        test - assertMatch(
          Equality(
            Literal(Value.Integer(5)),
            Literal(Value.Integer(6))
          ).evaluate
        ) { case Right((_, Value.Bool(false))) => }

        test - assertMatch(
          Equality(
            Literal(Value.Integer(5)),
            Literal(Value.Bool(true))
          ).evaluate
        ) { case Right((_, Value.Bool(false))) => }
      }
    }

    test("greater") {

      test("true") - assertMatch(
        Greater(
          Literal(Value.Integer(1)),
          Literal(Value.Integer(0))
        ).evaluate
      ) { case Right((_, Value.Bool(true))) => }

      test("false") - assertMatch(
        Greater(
          Literal(Value.Integer(0)),
          Literal(Value.Integer(1))
        ).evaluate
      ) { case Right((_, Value.Bool(false))) => }
    }

    test("greaterEqual") {

      test("true") {

        test - assertMatch(
          GreaterEqual(
            Literal(Value.Integer(1)),
            Literal(Value.Integer(0))
          ).evaluate
        ) { case Right((_, Value.Bool(true))) => }

        test - assertMatch(
          GreaterEqual(
            Literal(Value.Integer(1)),
            Literal(Value.Integer(1))
          ).evaluate
        ) { case Right((_, Value.Bool(true))) => }
      }

      test("false") - assertMatch(
        GreaterEqual(
          Literal(Value.Integer(0)),
          Literal(Value.Integer(1))
        ).evaluate
      ) { case Right((_, Value.Bool(false))) => }
    }

    test("less") {

      test("true") - assertMatch(
        Less(
          Literal(Value.Integer(0)),
          Literal(Value.Integer(1))
        ).evaluate
      ) { case Right((_, Value.Bool(true))) => }

      test("false") - assertMatch(
        Less(
          Literal(Value.Integer(1)),
          Literal(Value.Integer(0))
        ).evaluate
      ) { case Right((_, Value.Bool(false))) => }
    }

    test("lessEqual") {

      test("true") {

        test - assertMatch(
          LessEqual(
            Literal(Value.Integer(0)),
            Literal(Value.Integer(1))
          ).evaluate
        ) { case Right((_, Value.Bool(true))) => }

        test - assertMatch(
          LessEqual(
            Literal(Value.Integer(1)),
            Literal(Value.Integer(1))
          ).evaluate
        ) { case Right((_, Value.Bool(true))) => }
      }

      test("false") - assertMatch(
        LessEqual(
          Literal(Value.Integer(1)),
          Literal(Value.Integer(0))
        ).evaluate
      ) { case Right((_, Value.Bool(false))) => }
    }

    test("not") {
      test - assertMatch(
        Not(
          Literal(Value.Bool(true))
        ).evaluate
      ) { case Right((_, Value.Bool(false))) => }

      test - assertMatch(
        Not(
          Literal(Value.Bool(false))
        ).evaluate
      ) { case Right((_, Value.Bool(true))) => }
    }

    test("and") {

      test("true") - assertMatch(
        And(
          Literal(Value.Bool(true)),
          Literal(Value.Bool(true))
        ).evaluate
      ) { case Right((_, Value.Bool(true))) => }

      test("false") {

        test - assertMatch(
          And(
            Literal(Value.Bool(false)),
            Literal(Value.Bool(false))
          ).evaluate
        ) { case Right((_, Value.Bool(false))) => }

        test - assertMatch(
          And(
            Literal(Value.Bool(true)),
            Literal(Value.Bool(false))
          ).evaluate
        ) { case Right((_, Value.Bool(false))) => }

        assertMatch(
          And(
            Literal(Value.Bool(false)),
            Literal(Value.Bool(true))
          ).evaluate
        ) { case Right((_, Value.Bool(false))) => }
      }
    }

    test("or") {

      test("true") {

        test - assertMatch(
          Or(
            Literal(Value.Bool(true)),
            Literal(Value.Bool(true))
          ).evaluate
        ) { case Right((_, Value.Bool(true))) => }

        test - assertMatch(
          Or(
            Literal(Value.Bool(true)),
            Literal(Value.Bool(false))
          ).evaluate
        ) { case Right((_, Value.Bool(true))) => }

        test - assertMatch(
          Or(
            Literal(Value.Bool(false)),
            Literal(Value.Bool(true))
          ).evaluate
        ) { case Right((_, Value.Bool(true))) => }
      }

      test("false") - assertMatch(
        Or(
          Literal(Value.Bool(false)),
          Literal(Value.Bool(false))
        ).evaluate
      ) { case Right((_, Value.Bool(false))) => }
    }

    test("variableCall") {

      given Context = Context(
        Scope
          .empty
          .withDeclaration("a", CYType.Integer, Value.Integer(2)),
        List.empty,
        None
      )

      test("existing") - assertMatch(VariableCall("a").evaluate) { case Right((_, Value.Integer(2))) => }
      test("unknown") - assertMatch(VariableCall("b").evaluate) { case Left(_) => }
    }

    test("variableAssignment") {

      given Context = Context(
        Scope
          .empty
          .withDeclaration("a", CYType.Integer, Value.Integer(2)),
        List.empty,
        None
      )

      test("existing") {
        val result =
          VariableAssignment("a", Literal(Value.Integer(2)))
            .evaluate
            .map(_._1.scope.variables("a"))

        assertMatch(result) { case Right(Variable(CYType.Integer, Value.Integer(2), _, _)) => }
      }

      test("unknown") - assertMatch(VariableAssignment("b", Literal(Value.Integer(2))).evaluate) { case Left(_) => }
    }

    test("arrayCall") {

      val array = Literal(Value.Array(Array(Value.Character('a'), Value.Character('b'))))

      test("inBounds") - assertMatch(ArrayCall(array, Literal(Value.Integer(1))).evaluate) { case Right((_, Value.Character('b'))) => }
      test("outOfBounds") - assertMatch(ArrayCall(array, Literal(Value.Integer(2))).evaluate) { case Left(_) => }
      test("negative") - assertMatch(ArrayCall(array, Literal(Value.Integer(-1))).evaluate) { case Left(_) => }
    }

    test("arrayAssignment") {
      val array = Value.Array(Array(Value.Character('a'), Value.Character('b')))

      given Context = Context(
        Scope
          .empty
          .withDeclaration("array", CYType.Array(CYType.Character, None), array),
        List.empty,
        None
      )

      test("inBounds") - assertMatch(
        ArrayAssignment(
          Literal(array),
          Literal(Value.Integer(1)),
          Literal(Value.Character('c'))
        )
          .evaluate
          .map(_._1.scope.variables("array").value)
      ) { case Right(Value.Array(Array(Value.Character('a'), Value.Character('c')))) => }

      test("outOfBounds") - assertMatch(ArrayAssignment(Literal(array), Literal(Value.Integer(2)), Literal(Value.Character('c'))).evaluate) { case Left(_) => }
      test("negative") - assertMatch(ArrayAssignment(Literal(array), Literal(Value.Integer(-1)), Literal(Value.Character('c'))).evaluate) { case Left(_) => }
    }

    test("structureCall") {

      val instance = Literal(Value.StructureInstance("Point", mutable.Map(
        "x" -> Variable(CYType.Real, Value.Real(0), true, 0),
        "y" -> Variable(CYType.Real, Value.Real(0), true, 0)
      )))

      test("valid") - assertMatch(StructureCall(instance, "x").evaluate) { case Right((_, Value.Real(0))) => }
      test("unknown") - assertMatch(StructureCall(instance, "z").evaluate) { case Left(_) => }
    }

    test("structureAssignment") {

      val instance = Value.StructureInstance("Point", mutable.Map(
        "x" -> Variable(CYType.Real, Value.Real(0), true, 0),
        "y" -> Variable(CYType.Real, Value.Real(0), true, 0)
      ))

      given Context = Context(
        Scope
          .empty
          .withDeclaration("point", CYType.StructureInstance("Point"), instance),
        List.empty,
        None
      )


      test("valid") - assertMatch(
        StructureAssignment(Literal(instance), "x", Literal(Value.Real(1)))
          .evaluate
          .map(_._1.scope.variables("point").value)
      ) { case Right(Value.StructureInstance(_, map)) if map("x").value equals Value.Real(1) => }

      test("unknown") - assertMatch(StructureCall(Literal(instance), "z").evaluate) { case Left(_) => }
    }

    test("functionCall") {

      given Context = Context(
        Scope
          .empty
          .withFunction("a", CYFunction(CYType.Integer, List.empty, Map.empty, Literal(Value.Integer(2))))
          .withFunction(
            "b",
            CYFunction(
              CYType.Integer,
              List(Parameter("x", CYType.Integer)),
              Map.empty,
              VariableCall("x")
            )
          ),
        List.empty,
        None
      )

      test("constant") - assertMatch(FunctionCall("a", List.empty).evaluate) { case Right((_, Value.Integer(2))) => }
      test("withParameter") {
        test("ok") - assertMatch(FunctionCall("b", List(Literal(Value.Integer(5)))).evaluate) { case Right((_, Value.Integer(5))) => }
        test("not enough") - assertMatch(FunctionCall("b", List.empty).evaluate) { case Left(_) => }
        test("too many") - assertMatch(FunctionCall("b", List(Literal(Value.Integer(5)), Literal(Value.Integer(5)))).evaluate) { case Left(_) => }
      }
      test("unknown") - assertMatch(FunctionCall("unknown", List.empty).evaluate) { case Left(_) => }
    }

    test("forLoop") {
      given Context = Context(
        Scope
          .empty
          .withDeclaration("x", CYType.Integer, Value.Integer(0))
          .withDeclaration("i", CYType.Integer, Value.Void),
        List.empty,
        None
      )

      val result =
        ForLoop(
          "i",
          Literal(Value.Integer(0)),
          Literal(Value.Integer(10)),
          Literal(Value.Integer(1)),
          VariableAssignment(
            "x",
            Addition(
              VariableCall("x"),
              VariableCall("i")
            )
          )
        ).evaluate

      assertMatch(result.map(_._1.scope.variables("x"))) { case Right(Variable(CYType.Integer, Value.Integer(45), _, _)) => }
    }

    test("whileLoop") {

      given Context = Context(
        Scope
          .empty
          .withDeclaration("x", CYType.Integer, Value.Integer(0))
          .withDeclaration("i", CYType.Integer, Value.Integer(0)),
        List.empty,
        None
      )

      val result = WhileLoop(
        condition = Less(VariableCall("i"), Literal(Value.Integer(10))),
        expression = Tree(List(
          VariableAssignment(
            "x",
            Addition(
              VariableCall("x"),
              VariableCall("i")
            )
          ),
          VariableAssignment(
            "i",
            Addition(
              VariableCall("i"),
              Literal(Value.Integer(1))
            )
          )
        ))
      ).evaluate

      assertMatch(result.map(_._1.scope.variables("x"))) { case Right(Variable(CYType.Integer, Value.Integer(45), _, _)) => }
    }

    test("if") {

      given Context = Context(
        Scope
          .empty
          .withDeclaration("x", CYType.Integer, Value.Integer(0)),
        List.empty,
        None
      )

      def ast(condition: Boolean, elseCond: Expression) = If(
        Literal(Value.Bool(condition)),
        VariableAssignment("x", Literal(Value.Integer(1))),
        elseCond
      )

      test - assertMatch(ast(true, Empty).evaluate.map(_._1.scope.variables("x"))) {
        case Right(Variable(CYType.Integer, Value.Integer(1), _, _)) =>
      }

      test - assertMatch(ast(false, Empty).evaluate.map(_._1.scope.variables("x"))) {
        case Right(Variable(CYType.Integer, Value.Integer(0), _, _)) =>
      }

      test - assertMatch(ast(false, VariableAssignment("x", Literal(Value.Integer(-1)))).evaluate.map(_._1.scope.variables("x"))) {
        case Right(Variable(CYType.Integer, Value.Integer(-1), _, _)) =>
      }
    }

    test("tree") {

      test - assertMatch(
        Tree(List(
          Return(Literal(Value.Integer(0)))
        )).evaluate
      ) { case Right((_, Value.Integer(0))) => }

      test - assertMatch(Tree(List.empty).evaluate) { case Right((_, Value.Void)) => }

      test - assertMatch(Tree(List(Literal(Value.Integer(0)))).evaluate) { case Right((_, Value.Void)) => }

    }

    test("constantDecl") - assertMatch(
      ConstantDeclaration(
        "TEST",
        Literal(Value.Integer(1))
      )
        .evaluate
        .map(_._1.scope.variables("TEST"))
    ) { case Right(Variable(CYType.Integer, Value.Integer(1), false, _)) => }

    test("enumerationDecl") {

      test("type") - assertMatch(
        EnumerationDeclaration(
          "Color",
          List("RED", "GREEN", "BLUE")
        )
          .evaluate
          .map(_._1.scope.enumerations("Color"))
      ) { case Right(Enumeration("Color", _)) => }

      test("fields") - assertMatch(
        EnumerationDeclaration(
          "Color",
          List("RED", "GREEN", "BLUE")
        )
          .evaluate
          .map(_._1.scope.variables("RED"))
      ) { case Right(Variable(CYType.EnumerationField("Color"), Value.EnumerationField("Color", "RED"), false, _)) => }
    }

    test("structureDecl") - assertMatch(
      StructureDeclaration(
        "Point",
        List(
          Parameter("x", CYType.Real),
          Parameter("y", CYType.Real)
        )
      )
        .evaluate
        .map(_._1.scope.structures("Point"))
    ) { case Right(Structure("Point", _)) => }

    test("functionDecl") - assertMatch(
      FunctionDeclaration(
        "facto",
        CYType.Integer,
        List(Parameter("x", CYType.Integer)),
        Body(
          List(Parameter("res", CYType.Integer)),
          Empty
        )
      )
        .evaluate
        .map(_._1.scope.functions("facto"))
    ) { case Right(CYFunction.UserDefined(CYType.Integer, List(Parameter("x", CYType.Integer)), _, _)) => }

    test("program") {

      val program = ProgramDeclaration(
        name = "test",
        declarations = List(
          FunctionDeclaration(
            name = "factorial",
            tpe = CYType.Integer,
            parameters = List(Parameter("x", CYType.Integer)),
            body = Body(
              variables = List(Parameter("result", CYType.Integer)),
              expression = Tree(List(
                VariableAssignment(
                  name = "result",
                  expression = Literal(Value.Integer(1))
                ),
                ForLoop(
                  name = "i",
                  from = Literal(Value.Integer(1)),
                  to = Addition(VariableCall("x"), Literal(Value.Integer(1))),
                  step = Literal(Value.Integer(1)),
                  expression = VariableAssignment(
                    name = "result",
                    expression = Multiplication(VariableCall("result"), VariableCall("i"))
                  )
                ),
                Return(VariableCall("result"))
              ))
            )
          )
        ),
        body = Body(
          variables = List(
            Parameter("i", CYType.Integer)
          ),
          expression = FunctionCall("factorial", List(Literal(Value.Integer(5))))
        )
      )

      assertMatch(program.evaluate) {
        case Right((_, Value.Integer(120))) =>
      }
    }
  }
}