// Generated from Arboretum.g4 by ANTLR 4.7.2
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link ArboretumParser}.
 */
public interface ArboretumListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by the {@code numericAtomExp}
	 * labeled alternative in {@link ArboretumParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterNumericAtomExp(ArboretumParser.NumericAtomExpContext ctx);
	/**
	 * Exit a parse tree produced by the {@code numericAtomExp}
	 * labeled alternative in {@link ArboretumParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitNumericAtomExp(ArboretumParser.NumericAtomExpContext ctx);
	/**
	 * Enter a parse tree produced by the {@code powerExp}
	 * labeled alternative in {@link ArboretumParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterPowerExp(ArboretumParser.PowerExpContext ctx);
	/**
	 * Exit a parse tree produced by the {@code powerExp}
	 * labeled alternative in {@link ArboretumParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitPowerExp(ArboretumParser.PowerExpContext ctx);
	/**
	 * Enter a parse tree produced by the {@code mulDivExp}
	 * labeled alternative in {@link ArboretumParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterMulDivExp(ArboretumParser.MulDivExpContext ctx);
	/**
	 * Exit a parse tree produced by the {@code mulDivExp}
	 * labeled alternative in {@link ArboretumParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitMulDivExp(ArboretumParser.MulDivExpContext ctx);
	/**
	 * Enter a parse tree produced by the {@code parenthesisExp}
	 * labeled alternative in {@link ArboretumParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterParenthesisExp(ArboretumParser.ParenthesisExpContext ctx);
	/**
	 * Exit a parse tree produced by the {@code parenthesisExp}
	 * labeled alternative in {@link ArboretumParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitParenthesisExp(ArboretumParser.ParenthesisExpContext ctx);
	/**
	 * Enter a parse tree produced by the {@code idAtomExp}
	 * labeled alternative in {@link ArboretumParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterIdAtomExp(ArboretumParser.IdAtomExpContext ctx);
	/**
	 * Exit a parse tree produced by the {@code idAtomExp}
	 * labeled alternative in {@link ArboretumParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitIdAtomExp(ArboretumParser.IdAtomExpContext ctx);
	/**
	 * Enter a parse tree produced by the {@code addSubExp}
	 * labeled alternative in {@link ArboretumParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterAddSubExp(ArboretumParser.AddSubExpContext ctx);
	/**
	 * Exit a parse tree produced by the {@code addSubExp}
	 * labeled alternative in {@link ArboretumParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitAddSubExp(ArboretumParser.AddSubExpContext ctx);
	/**
	 * Enter a parse tree produced by the {@code functionExp}
	 * labeled alternative in {@link ArboretumParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterFunctionExp(ArboretumParser.FunctionExpContext ctx);
	/**
	 * Exit a parse tree produced by the {@code functionExp}
	 * labeled alternative in {@link ArboretumParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitFunctionExp(ArboretumParser.FunctionExpContext ctx);
}