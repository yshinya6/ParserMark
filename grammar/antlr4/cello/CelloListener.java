// Generated from Cello.g4 by ANTLR 4.5.3
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link CelloParser}.
 */
public interface CelloListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link CelloParser#topLevel}.
	 * @param ctx the parse tree
	 */
	void enterTopLevel(CelloParser.TopLevelContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#topLevel}.
	 * @param ctx the parse tree
	 */
	void exitTopLevel(CelloParser.TopLevelContext ctx);
	/**
	 * Enter a parse tree produced by {@link CelloParser#declaration}.
	 * @param ctx the parse tree
	 */
	void enterDeclaration(CelloParser.DeclarationContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#declaration}.
	 * @param ctx the parse tree
	 */
	void exitDeclaration(CelloParser.DeclarationContext ctx);
	/**
	 * Enter a parse tree produced by {@link CelloParser#functionDeclaration}.
	 * @param ctx the parse tree
	 */
	void enterFunctionDeclaration(CelloParser.FunctionDeclarationContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#functionDeclaration}.
	 * @param ctx the parse tree
	 */
	void exitFunctionDeclaration(CelloParser.FunctionDeclarationContext ctx);
	/**
	 * Enter a parse tree produced by {@link CelloParser#functionParamList}.
	 * @param ctx the parse tree
	 */
	void enterFunctionParamList(CelloParser.FunctionParamListContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#functionParamList}.
	 * @param ctx the parse tree
	 */
	void exitFunctionParamList(CelloParser.FunctionParamListContext ctx);
	/**
	 * Enter a parse tree produced by {@link CelloParser#functionParam}.
	 * @param ctx the parse tree
	 */
	void enterFunctionParam(CelloParser.FunctionParamContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#functionParam}.
	 * @param ctx the parse tree
	 */
	void exitFunctionParam(CelloParser.FunctionParamContext ctx);
	/**
	 * Enter a parse tree produced by {@link CelloParser#variableDeclaration}.
	 * @param ctx the parse tree
	 */
	void enterVariableDeclaration(CelloParser.VariableDeclarationContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#variableDeclaration}.
	 * @param ctx the parse tree
	 */
	void exitVariableDeclaration(CelloParser.VariableDeclarationContext ctx);
	/**
	 * Enter a parse tree produced by {@link CelloParser#variableList}.
	 * @param ctx the parse tree
	 */
	void enterVariableList(CelloParser.VariableListContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#variableList}.
	 * @param ctx the parse tree
	 */
	void exitVariableList(CelloParser.VariableListContext ctx);
	/**
	 * Enter a parse tree produced by {@link CelloParser#initDecl}.
	 * @param ctx the parse tree
	 */
	void enterInitDecl(CelloParser.InitDeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#initDecl}.
	 * @param ctx the parse tree
	 */
	void exitInitDecl(CelloParser.InitDeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link CelloParser#initializer}.
	 * @param ctx the parse tree
	 */
	void enterInitializer(CelloParser.InitializerContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#initializer}.
	 * @param ctx the parse tree
	 */
	void exitInitializer(CelloParser.InitializerContext ctx);
	/**
	 * Enter a parse tree produced by {@link CelloParser#type}.
	 * @param ctx the parse tree
	 */
	void enterType(CelloParser.TypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#type}.
	 * @param ctx the parse tree
	 */
	void exitType(CelloParser.TypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link CelloParser#primitiveType}.
	 * @param ctx the parse tree
	 */
	void enterPrimitiveType(CelloParser.PrimitiveTypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#primitiveType}.
	 * @param ctx the parse tree
	 */
	void exitPrimitiveType(CelloParser.PrimitiveTypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link CelloParser#block}.
	 * @param ctx the parse tree
	 */
	void enterBlock(CelloParser.BlockContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#block}.
	 * @param ctx the parse tree
	 */
	void exitBlock(CelloParser.BlockContext ctx);
	/**
	 * Enter a parse tree produced by {@link CelloParser#statement}.
	 * @param ctx the parse tree
	 */
	void enterStatement(CelloParser.StatementContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#statement}.
	 * @param ctx the parse tree
	 */
	void exitStatement(CelloParser.StatementContext ctx);
	/**
	 * Enter a parse tree produced by {@link CelloParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterExpression(CelloParser.ExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitExpression(CelloParser.ExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CelloParser#assignmentExpression}.
	 * @param ctx the parse tree
	 */
	void enterAssignmentExpression(CelloParser.AssignmentExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#assignmentExpression}.
	 * @param ctx the parse tree
	 */
	void exitAssignmentExpression(CelloParser.AssignmentExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CelloParser#conditionalExpression}.
	 * @param ctx the parse tree
	 */
	void enterConditionalExpression(CelloParser.ConditionalExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#conditionalExpression}.
	 * @param ctx the parse tree
	 */
	void exitConditionalExpression(CelloParser.ConditionalExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CelloParser#equalityExpression}.
	 * @param ctx the parse tree
	 */
	void enterEqualityExpression(CelloParser.EqualityExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#equalityExpression}.
	 * @param ctx the parse tree
	 */
	void exitEqualityExpression(CelloParser.EqualityExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CelloParser#relationalExpression}.
	 * @param ctx the parse tree
	 */
	void enterRelationalExpression(CelloParser.RelationalExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#relationalExpression}.
	 * @param ctx the parse tree
	 */
	void exitRelationalExpression(CelloParser.RelationalExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CelloParser#unaryExpression}.
	 * @param ctx the parse tree
	 */
	void enterUnaryExpression(CelloParser.UnaryExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#unaryExpression}.
	 * @param ctx the parse tree
	 */
	void exitUnaryExpression(CelloParser.UnaryExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CelloParser#postfixExpression}.
	 * @param ctx the parse tree
	 */
	void enterPostfixExpression(CelloParser.PostfixExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#postfixExpression}.
	 * @param ctx the parse tree
	 */
	void exitPostfixExpression(CelloParser.PostfixExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CelloParser#primaryExpression}.
	 * @param ctx the parse tree
	 */
	void enterPrimaryExpression(CelloParser.PrimaryExpressionContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#primaryExpression}.
	 * @param ctx the parse tree
	 */
	void exitPrimaryExpression(CelloParser.PrimaryExpressionContext ctx);
	/**
	 * Enter a parse tree produced by {@link CelloParser#methodInvocation}.
	 * @param ctx the parse tree
	 */
	void enterMethodInvocation(CelloParser.MethodInvocationContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#methodInvocation}.
	 * @param ctx the parse tree
	 */
	void exitMethodInvocation(CelloParser.MethodInvocationContext ctx);
	/**
	 * Enter a parse tree produced by {@link CelloParser#expressionList}.
	 * @param ctx the parse tree
	 */
	void enterExpressionList(CelloParser.ExpressionListContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#expressionList}.
	 * @param ctx the parse tree
	 */
	void exitExpressionList(CelloParser.ExpressionListContext ctx);
	/**
	 * Enter a parse tree produced by {@link CelloParser#literal}.
	 * @param ctx the parse tree
	 */
	void enterLiteral(CelloParser.LiteralContext ctx);
	/**
	 * Exit a parse tree produced by {@link CelloParser#literal}.
	 * @param ctx the parse tree
	 */
	void exitLiteral(CelloParser.LiteralContext ctx);
}