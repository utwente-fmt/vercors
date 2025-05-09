// Generated from /home/alexander/vercors/src/parsers/antlr4/TestNoSpecParser.g4 by ANTLR 4.13.2
package antlr4;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue", "this-escape"})
public class TestNoSpecParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.13.2", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		RULE_valEmp = 0, RULE_valContractClause = 1, RULE_valStatement = 2, RULE_valWithThen = 3, 
		RULE_valPrimary = 4, RULE_valReserved = 5, RULE_valType = 6, RULE_valDeclaration = 7, 
		RULE_valModifier = 8, RULE_valEmbedContract = 9, RULE_valEmbedStatementBlock = 10, 
		RULE_valEmbedWithThenBlock = 11, RULE_valEmbedWithThen = 12, RULE_valEmbedDeclarationBlock = 13, 
		RULE_valMulOp = 14, RULE_valAndOp = 15, RULE_valImpOp = 16;
	private static String[] makeRuleNames() {
		return new String[] {
			"valEmp", "valContractClause", "valStatement", "valWithThen", "valPrimary", 
			"valReserved", "valType", "valDeclaration", "valModifier", "valEmbedContract", 
			"valEmbedStatementBlock", "valEmbedWithThenBlock", "valEmbedWithThen", 
			"valEmbedDeclarationBlock", "valMulOp", "valAndOp", "valImpOp"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "TestNoSpecParser.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public TestNoSpecParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ValEmpContext extends ParserRuleContext {
		public List<TerminalNode> EOF() { return getTokens(TestNoSpecParser.EOF); }
		public TerminalNode EOF(int i) {
			return getToken(TestNoSpecParser.EOF, i);
		}
		public ValEmpContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_valEmp; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).enterValEmp(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).exitValEmp(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TestNoSpecParserVisitor ) return ((TestNoSpecParserVisitor<? extends T>)visitor).visitValEmp(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ValEmpContext valEmp() throws RecognitionException {
		ValEmpContext _localctx = new ValEmpContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_valEmp);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(34);
			match(EOF);
			setState(35);
			match(EOF);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ValContractClauseContext extends ParserRuleContext {
		public ValEmpContext valEmp() {
			return getRuleContext(ValEmpContext.class,0);
		}
		public ValContractClauseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_valContractClause; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).enterValContractClause(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).exitValContractClause(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TestNoSpecParserVisitor ) return ((TestNoSpecParserVisitor<? extends T>)visitor).visitValContractClause(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ValContractClauseContext valContractClause() throws RecognitionException {
		ValContractClauseContext _localctx = new ValContractClauseContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_valContractClause);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(37);
			valEmp();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ValStatementContext extends ParserRuleContext {
		public ValEmpContext valEmp() {
			return getRuleContext(ValEmpContext.class,0);
		}
		public ValStatementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_valStatement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).enterValStatement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).exitValStatement(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TestNoSpecParserVisitor ) return ((TestNoSpecParserVisitor<? extends T>)visitor).visitValStatement(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ValStatementContext valStatement() throws RecognitionException {
		ValStatementContext _localctx = new ValStatementContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_valStatement);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(39);
			valEmp();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ValWithThenContext extends ParserRuleContext {
		public ValEmpContext valEmp() {
			return getRuleContext(ValEmpContext.class,0);
		}
		public ValWithThenContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_valWithThen; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).enterValWithThen(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).exitValWithThen(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TestNoSpecParserVisitor ) return ((TestNoSpecParserVisitor<? extends T>)visitor).visitValWithThen(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ValWithThenContext valWithThen() throws RecognitionException {
		ValWithThenContext _localctx = new ValWithThenContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_valWithThen);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(41);
			valEmp();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ValPrimaryContext extends ParserRuleContext {
		public ValEmpContext valEmp() {
			return getRuleContext(ValEmpContext.class,0);
		}
		public ValPrimaryContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_valPrimary; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).enterValPrimary(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).exitValPrimary(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TestNoSpecParserVisitor ) return ((TestNoSpecParserVisitor<? extends T>)visitor).visitValPrimary(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ValPrimaryContext valPrimary() throws RecognitionException {
		ValPrimaryContext _localctx = new ValPrimaryContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_valPrimary);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(43);
			valEmp();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ValReservedContext extends ParserRuleContext {
		public ValEmpContext valEmp() {
			return getRuleContext(ValEmpContext.class,0);
		}
		public ValReservedContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_valReserved; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).enterValReserved(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).exitValReserved(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TestNoSpecParserVisitor ) return ((TestNoSpecParserVisitor<? extends T>)visitor).visitValReserved(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ValReservedContext valReserved() throws RecognitionException {
		ValReservedContext _localctx = new ValReservedContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_valReserved);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(45);
			valEmp();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ValTypeContext extends ParserRuleContext {
		public ValEmpContext valEmp() {
			return getRuleContext(ValEmpContext.class,0);
		}
		public ValTypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_valType; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).enterValType(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).exitValType(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TestNoSpecParserVisitor ) return ((TestNoSpecParserVisitor<? extends T>)visitor).visitValType(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ValTypeContext valType() throws RecognitionException {
		ValTypeContext _localctx = new ValTypeContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_valType);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(47);
			valEmp();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ValDeclarationContext extends ParserRuleContext {
		public ValEmpContext valEmp() {
			return getRuleContext(ValEmpContext.class,0);
		}
		public ValDeclarationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_valDeclaration; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).enterValDeclaration(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).exitValDeclaration(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TestNoSpecParserVisitor ) return ((TestNoSpecParserVisitor<? extends T>)visitor).visitValDeclaration(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ValDeclarationContext valDeclaration() throws RecognitionException {
		ValDeclarationContext _localctx = new ValDeclarationContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_valDeclaration);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(49);
			valEmp();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ValModifierContext extends ParserRuleContext {
		public ValEmpContext valEmp() {
			return getRuleContext(ValEmpContext.class,0);
		}
		public ValModifierContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_valModifier; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).enterValModifier(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).exitValModifier(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TestNoSpecParserVisitor ) return ((TestNoSpecParserVisitor<? extends T>)visitor).visitValModifier(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ValModifierContext valModifier() throws RecognitionException {
		ValModifierContext _localctx = new ValModifierContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_valModifier);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(51);
			valEmp();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ValEmbedContractContext extends ParserRuleContext {
		public ValEmpContext valEmp() {
			return getRuleContext(ValEmpContext.class,0);
		}
		public ValEmbedContractContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_valEmbedContract; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).enterValEmbedContract(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).exitValEmbedContract(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TestNoSpecParserVisitor ) return ((TestNoSpecParserVisitor<? extends T>)visitor).visitValEmbedContract(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ValEmbedContractContext valEmbedContract() throws RecognitionException {
		ValEmbedContractContext _localctx = new ValEmbedContractContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_valEmbedContract);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(53);
			valEmp();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ValEmbedStatementBlockContext extends ParserRuleContext {
		public ValEmpContext valEmp() {
			return getRuleContext(ValEmpContext.class,0);
		}
		public ValEmbedStatementBlockContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_valEmbedStatementBlock; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).enterValEmbedStatementBlock(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).exitValEmbedStatementBlock(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TestNoSpecParserVisitor ) return ((TestNoSpecParserVisitor<? extends T>)visitor).visitValEmbedStatementBlock(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ValEmbedStatementBlockContext valEmbedStatementBlock() throws RecognitionException {
		ValEmbedStatementBlockContext _localctx = new ValEmbedStatementBlockContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_valEmbedStatementBlock);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(55);
			valEmp();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ValEmbedWithThenBlockContext extends ParserRuleContext {
		public ValEmpContext valEmp() {
			return getRuleContext(ValEmpContext.class,0);
		}
		public ValEmbedWithThenBlockContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_valEmbedWithThenBlock; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).enterValEmbedWithThenBlock(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).exitValEmbedWithThenBlock(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TestNoSpecParserVisitor ) return ((TestNoSpecParserVisitor<? extends T>)visitor).visitValEmbedWithThenBlock(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ValEmbedWithThenBlockContext valEmbedWithThenBlock() throws RecognitionException {
		ValEmbedWithThenBlockContext _localctx = new ValEmbedWithThenBlockContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_valEmbedWithThenBlock);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(57);
			valEmp();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ValEmbedWithThenContext extends ParserRuleContext {
		public ValEmpContext valEmp() {
			return getRuleContext(ValEmpContext.class,0);
		}
		public ValEmbedWithThenContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_valEmbedWithThen; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).enterValEmbedWithThen(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).exitValEmbedWithThen(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TestNoSpecParserVisitor ) return ((TestNoSpecParserVisitor<? extends T>)visitor).visitValEmbedWithThen(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ValEmbedWithThenContext valEmbedWithThen() throws RecognitionException {
		ValEmbedWithThenContext _localctx = new ValEmbedWithThenContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_valEmbedWithThen);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(59);
			valEmp();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ValEmbedDeclarationBlockContext extends ParserRuleContext {
		public ValEmpContext valEmp() {
			return getRuleContext(ValEmpContext.class,0);
		}
		public ValEmbedDeclarationBlockContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_valEmbedDeclarationBlock; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).enterValEmbedDeclarationBlock(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).exitValEmbedDeclarationBlock(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TestNoSpecParserVisitor ) return ((TestNoSpecParserVisitor<? extends T>)visitor).visitValEmbedDeclarationBlock(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ValEmbedDeclarationBlockContext valEmbedDeclarationBlock() throws RecognitionException {
		ValEmbedDeclarationBlockContext _localctx = new ValEmbedDeclarationBlockContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_valEmbedDeclarationBlock);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(61);
			valEmp();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ValMulOpContext extends ParserRuleContext {
		public ValEmpContext valEmp() {
			return getRuleContext(ValEmpContext.class,0);
		}
		public ValMulOpContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_valMulOp; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).enterValMulOp(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).exitValMulOp(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TestNoSpecParserVisitor ) return ((TestNoSpecParserVisitor<? extends T>)visitor).visitValMulOp(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ValMulOpContext valMulOp() throws RecognitionException {
		ValMulOpContext _localctx = new ValMulOpContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_valMulOp);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(63);
			valEmp();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ValAndOpContext extends ParserRuleContext {
		public ValEmpContext valEmp() {
			return getRuleContext(ValEmpContext.class,0);
		}
		public ValAndOpContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_valAndOp; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).enterValAndOp(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).exitValAndOp(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TestNoSpecParserVisitor ) return ((TestNoSpecParserVisitor<? extends T>)visitor).visitValAndOp(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ValAndOpContext valAndOp() throws RecognitionException {
		ValAndOpContext _localctx = new ValAndOpContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_valAndOp);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(65);
			valEmp();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ValImpOpContext extends ParserRuleContext {
		public ValEmpContext valEmp() {
			return getRuleContext(ValEmpContext.class,0);
		}
		public ValImpOpContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_valImpOp; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).enterValImpOp(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TestNoSpecParserListener ) ((TestNoSpecParserListener)listener).exitValImpOp(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TestNoSpecParserVisitor ) return ((TestNoSpecParserVisitor<? extends T>)visitor).visitValImpOp(this);
			else return visitor.visitChildren(this);
		}
	}

	public final ValImpOpContext valImpOp() throws RecognitionException {
		ValImpOpContext _localctx = new ValImpOpContext(_ctx, getState());
		enterRule(_localctx, 32, RULE_valImpOp);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(67);
			valEmp();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\u0004\u0001\u0000F\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001\u0002"+
		"\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004\u0007\u0004\u0002"+
		"\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007\u0007\u0007\u0002"+
		"\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0002\u000b\u0007\u000b\u0002"+
		"\f\u0007\f\u0002\r\u0007\r\u0002\u000e\u0007\u000e\u0002\u000f\u0007\u000f"+
		"\u0002\u0010\u0007\u0010\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0001"+
		"\u0001\u0001\u0001\u0002\u0001\u0002\u0001\u0003\u0001\u0003\u0001\u0004"+
		"\u0001\u0004\u0001\u0005\u0001\u0005\u0001\u0006\u0001\u0006\u0001\u0007"+
		"\u0001\u0007\u0001\b\u0001\b\u0001\t\u0001\t\u0001\n\u0001\n\u0001\u000b"+
		"\u0001\u000b\u0001\f\u0001\f\u0001\r\u0001\r\u0001\u000e\u0001\u000e\u0001"+
		"\u000f\u0001\u000f\u0001\u0010\u0001\u0010\u0001\u0010\u0000\u0000\u0011"+
		"\u0000\u0002\u0004\u0006\b\n\f\u000e\u0010\u0012\u0014\u0016\u0018\u001a"+
		"\u001c\u001e \u0000\u00004\u0000\"\u0001\u0000\u0000\u0000\u0002%\u0001"+
		"\u0000\u0000\u0000\u0004\'\u0001\u0000\u0000\u0000\u0006)\u0001\u0000"+
		"\u0000\u0000\b+\u0001\u0000\u0000\u0000\n-\u0001\u0000\u0000\u0000\f/"+
		"\u0001\u0000\u0000\u0000\u000e1\u0001\u0000\u0000\u0000\u00103\u0001\u0000"+
		"\u0000\u0000\u00125\u0001\u0000\u0000\u0000\u00147\u0001\u0000\u0000\u0000"+
		"\u00169\u0001\u0000\u0000\u0000\u0018;\u0001\u0000\u0000\u0000\u001a="+
		"\u0001\u0000\u0000\u0000\u001c?\u0001\u0000\u0000\u0000\u001eA\u0001\u0000"+
		"\u0000\u0000 C\u0001\u0000\u0000\u0000\"#\u0005\u0000\u0000\u0001#$\u0005"+
		"\u0000\u0000\u0001$\u0001\u0001\u0000\u0000\u0000%&\u0003\u0000\u0000"+
		"\u0000&\u0003\u0001\u0000\u0000\u0000\'(\u0003\u0000\u0000\u0000(\u0005"+
		"\u0001\u0000\u0000\u0000)*\u0003\u0000\u0000\u0000*\u0007\u0001\u0000"+
		"\u0000\u0000+,\u0003\u0000\u0000\u0000,\t\u0001\u0000\u0000\u0000-.\u0003"+
		"\u0000\u0000\u0000.\u000b\u0001\u0000\u0000\u0000/0\u0003\u0000\u0000"+
		"\u00000\r\u0001\u0000\u0000\u000012\u0003\u0000\u0000\u00002\u000f\u0001"+
		"\u0000\u0000\u000034\u0003\u0000\u0000\u00004\u0011\u0001\u0000\u0000"+
		"\u000056\u0003\u0000\u0000\u00006\u0013\u0001\u0000\u0000\u000078\u0003"+
		"\u0000\u0000\u00008\u0015\u0001\u0000\u0000\u00009:\u0003\u0000\u0000"+
		"\u0000:\u0017\u0001\u0000\u0000\u0000;<\u0003\u0000\u0000\u0000<\u0019"+
		"\u0001\u0000\u0000\u0000=>\u0003\u0000\u0000\u0000>\u001b\u0001\u0000"+
		"\u0000\u0000?@\u0003\u0000\u0000\u0000@\u001d\u0001\u0000\u0000\u0000"+
		"AB\u0003\u0000\u0000\u0000B\u001f\u0001\u0000\u0000\u0000CD\u0003\u0000"+
		"\u0000\u0000D!\u0001\u0000\u0000\u0000\u0000";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}