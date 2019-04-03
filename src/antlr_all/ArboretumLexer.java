// Generated from Arboretum.g4 by ANTLR 4.7.2
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class ArboretumLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.7.2", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, ASTERISK=4, SLASH=5, PLUS=6, MINUS=7, ID=8, NAME=9, 
		NUMBER=10, WHITESPACE=11;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	private static String[] makeRuleNames() {
		return new String[] {
			"T__0", "T__1", "T__2", "LETTER", "DIGIT", "ASTERISK", "SLASH", "PLUS", 
			"MINUS", "ID", "NAME", "NUMBER", "WHITESPACE"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'('", "')'", "'^'", "'*'", "'/'", "'+'", "'-'", null, null, null, 
			"' '"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, null, null, "ASTERISK", "SLASH", "PLUS", "MINUS", "ID", "NAME", 
			"NUMBER", "WHITESPACE"
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


	public ArboretumLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "Arboretum.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\rH\b\1\4\2\t\2\4"+
		"\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t"+
		"\13\4\f\t\f\4\r\t\r\4\16\t\16\3\2\3\2\3\3\3\3\3\4\3\4\3\5\3\5\3\6\3\6"+
		"\3\7\3\7\3\b\3\b\3\t\3\t\3\n\3\n\3\13\3\13\3\13\3\f\6\f\64\n\f\r\f\16"+
		"\f\65\3\r\6\r9\n\r\r\r\16\r:\3\r\3\r\6\r?\n\r\r\r\16\r@\5\rC\n\r\3\16"+
		"\3\16\3\16\3\16\2\2\17\3\3\5\4\7\5\t\2\13\2\r\6\17\7\21\b\23\t\25\n\27"+
		"\13\31\f\33\r\3\2\4\4\2C\\c|\3\2\62;\2I\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3"+
		"\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2"+
		"\2\27\3\2\2\2\2\31\3\2\2\2\2\33\3\2\2\2\3\35\3\2\2\2\5\37\3\2\2\2\7!\3"+
		"\2\2\2\t#\3\2\2\2\13%\3\2\2\2\r\'\3\2\2\2\17)\3\2\2\2\21+\3\2\2\2\23-"+
		"\3\2\2\2\25/\3\2\2\2\27\63\3\2\2\2\318\3\2\2\2\33D\3\2\2\2\35\36\7*\2"+
		"\2\36\4\3\2\2\2\37 \7+\2\2 \6\3\2\2\2!\"\7`\2\2\"\b\3\2\2\2#$\t\2\2\2"+
		"$\n\3\2\2\2%&\t\3\2\2&\f\3\2\2\2\'(\7,\2\2(\16\3\2\2\2)*\7\61\2\2*\20"+
		"\3\2\2\2+,\7-\2\2,\22\3\2\2\2-.\7/\2\2.\24\3\2\2\2/\60\5\t\5\2\60\61\5"+
		"\13\6\2\61\26\3\2\2\2\62\64\5\t\5\2\63\62\3\2\2\2\64\65\3\2\2\2\65\63"+
		"\3\2\2\2\65\66\3\2\2\2\66\30\3\2\2\2\679\5\13\6\28\67\3\2\2\29:\3\2\2"+
		"\2:8\3\2\2\2:;\3\2\2\2;B\3\2\2\2<>\7\60\2\2=?\5\13\6\2>=\3\2\2\2?@\3\2"+
		"\2\2@>\3\2\2\2@A\3\2\2\2AC\3\2\2\2B<\3\2\2\2BC\3\2\2\2C\32\3\2\2\2DE\7"+
		"\"\2\2EF\3\2\2\2FG\b\16\2\2G\34\3\2\2\2\7\2\65:@B\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}