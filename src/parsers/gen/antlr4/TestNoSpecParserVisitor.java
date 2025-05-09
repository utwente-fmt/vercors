// Generated from /home/alexander/vercors/src/parsers/antlr4/TestNoSpecParser.g4 by ANTLR 4.13.2
package antlr4;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link TestNoSpecParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface TestNoSpecParserVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link TestNoSpecParser#valEmp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValEmp(TestNoSpecParser.ValEmpContext ctx);
	/**
	 * Visit a parse tree produced by {@link TestNoSpecParser#valContractClause}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValContractClause(TestNoSpecParser.ValContractClauseContext ctx);
	/**
	 * Visit a parse tree produced by {@link TestNoSpecParser#valStatement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValStatement(TestNoSpecParser.ValStatementContext ctx);
	/**
	 * Visit a parse tree produced by {@link TestNoSpecParser#valWithThen}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValWithThen(TestNoSpecParser.ValWithThenContext ctx);
	/**
	 * Visit a parse tree produced by {@link TestNoSpecParser#valPrimary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValPrimary(TestNoSpecParser.ValPrimaryContext ctx);
	/**
	 * Visit a parse tree produced by {@link TestNoSpecParser#valReserved}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValReserved(TestNoSpecParser.ValReservedContext ctx);
	/**
	 * Visit a parse tree produced by {@link TestNoSpecParser#valType}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValType(TestNoSpecParser.ValTypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link TestNoSpecParser#valDeclaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValDeclaration(TestNoSpecParser.ValDeclarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link TestNoSpecParser#valModifier}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValModifier(TestNoSpecParser.ValModifierContext ctx);
	/**
	 * Visit a parse tree produced by {@link TestNoSpecParser#valEmbedContract}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValEmbedContract(TestNoSpecParser.ValEmbedContractContext ctx);
	/**
	 * Visit a parse tree produced by {@link TestNoSpecParser#valEmbedStatementBlock}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValEmbedStatementBlock(TestNoSpecParser.ValEmbedStatementBlockContext ctx);
	/**
	 * Visit a parse tree produced by {@link TestNoSpecParser#valEmbedWithThenBlock}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValEmbedWithThenBlock(TestNoSpecParser.ValEmbedWithThenBlockContext ctx);
	/**
	 * Visit a parse tree produced by {@link TestNoSpecParser#valEmbedWithThen}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValEmbedWithThen(TestNoSpecParser.ValEmbedWithThenContext ctx);
	/**
	 * Visit a parse tree produced by {@link TestNoSpecParser#valEmbedDeclarationBlock}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValEmbedDeclarationBlock(TestNoSpecParser.ValEmbedDeclarationBlockContext ctx);
	/**
	 * Visit a parse tree produced by {@link TestNoSpecParser#valMulOp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValMulOp(TestNoSpecParser.ValMulOpContext ctx);
	/**
	 * Visit a parse tree produced by {@link TestNoSpecParser#valAndOp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValAndOp(TestNoSpecParser.ValAndOpContext ctx);
	/**
	 * Visit a parse tree produced by {@link TestNoSpecParser#valImpOp}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValImpOp(TestNoSpecParser.ValImpOpContext ctx);
}