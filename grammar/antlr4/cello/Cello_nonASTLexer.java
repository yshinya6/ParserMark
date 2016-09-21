// Generated from Cello_nonAST.g4 by ANTLR 4.5.3
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class Cello_nonASTLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.5.3", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		WhiteSpace=1, LineComment=2, BlockComment=3;
	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] ruleNames = {
		"WhiteSpace", "LineComment", "BlockComment", "NAME", "W", "Declaration", 
		"FunctionDeclaration", "FunctionParamList", "FunctionParam", "VariableDeclaration", 
		"VariableList", "InitDecl", "Initializer", "Type", "PrimitiveType", "Block", 
		"Statement", "Expression", "AssignmentExpression", "ConstantExpression", 
		"ConditionalExpression", "EqualityExpression", "RelationalExpression", 
		"AdditiveExpression", "MultiplicativeExpression", "UnaryExpression", "PostfixExpression", 
		"FunctionCall", "ArgumentExpressionList", "PrimaryExpression", "ExpressionList", 
		"Literal", "IntegerLiteral", "DIGIT", "NONZERODIGIT", "StringLiteral", 
		"STRING_CONTENT", "BooleanLiteral"
	};

	private static final String[] _LITERAL_NAMES = {
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, "WhiteSpace", "LineComment", "BlockComment"
	};
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


	public Cello_nonASTLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "Cello_nonAST.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\2\5\u0186\b\1\4\2\t"+
		"\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31\t\31"+
		"\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t \4!"+
		"\t!\4\"\t\"\4#\t#\4$\t$\4%\t%\4&\t&\4\'\t\'\3\2\6\2Q\n\2\r\2\16\2R\3\2"+
		"\3\2\3\3\3\3\3\3\3\3\7\3[\n\3\f\3\16\3^\13\3\3\3\3\3\3\4\3\4\3\4\3\4\7"+
		"\4f\n\4\f\4\16\4i\13\4\3\4\3\4\3\4\3\4\3\4\3\5\3\5\7\5r\n\5\f\5\16\5u"+
		"\13\5\3\6\3\6\3\7\3\7\5\7{\n\7\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\t\3\t\3\t"+
		"\7\t\u0087\n\t\f\t\16\t\u008a\13\t\5\t\u008c\n\t\3\n\3\n\3\n\3\13\3\13"+
		"\3\13\3\13\3\f\3\f\3\f\7\f\u0098\n\f\f\f\16\f\u009b\13\f\3\r\3\r\3\r\5"+
		"\r\u00a0\n\r\3\16\3\16\3\17\3\17\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3"+
		"\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\5\20\u00b6\n\20\3\21\3\21"+
		"\3\21\7\21\u00bb\n\21\f\21\16\21\u00be\13\21\3\21\3\21\3\22\3\22\3\22"+
		"\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\5\22\u00d0\n\22"+
		"\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\5\22\u00da\n\22\3\22\3\22\3\22"+
		"\3\22\3\22\3\22\3\22\5\22\u00e3\n\22\3\22\3\22\5\22\u00e7\n\22\3\22\3"+
		"\22\5\22\u00eb\n\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22\5\22"+
		"\u00f6\n\22\3\22\3\22\5\22\u00fa\n\22\3\22\3\22\3\22\3\22\3\22\3\22\3"+
		"\22\5\22\u0103\n\22\3\23\3\23\3\23\3\23\3\24\3\24\3\24\3\24\3\24\5\24"+
		"\u010e\n\24\3\25\3\25\3\26\3\26\3\26\3\26\3\26\5\26\u0117\n\26\3\26\3"+
		"\26\3\27\3\27\3\27\3\27\3\27\5\27\u0120\n\27\3\27\3\27\3\30\3\30\3\30"+
		"\3\30\3\31\3\31\3\31\3\31\3\32\3\32\3\32\3\32\3\32\3\32\3\33\3\33\3\33"+
		"\3\33\3\33\3\33\3\33\3\33\3\33\3\33\3\33\5\33\u013d\n\33\3\34\3\34\3\34"+
		"\3\35\3\35\3\35\3\35\3\36\3\36\3\36\7\36\u0149\n\36\f\36\16\36\u014c\13"+
		"\36\3\37\3\37\3\37\3\37\3\37\3\37\5\37\u0154\n\37\3 \3 \3 \7 \u0159\n"+
		" \f \16 \u015c\13 \3!\3!\3!\5!\u0161\n!\3\"\3\"\3\"\7\"\u0166\n\"\f\""+
		"\16\"\u0169\13\"\5\"\u016b\n\"\3#\3#\3$\3$\3%\3%\7%\u0173\n%\f%\16%\u0176"+
		"\13%\3%\3%\3&\3&\3\'\3\'\3\'\3\'\3\'\3\'\3\'\3\'\3\'\5\'\u0185\n\'\3g"+
		"\2(\3\3\5\4\7\5\t\2\13\2\r\2\17\2\21\2\23\2\25\2\27\2\31\2\33\2\35\2\37"+
		"\2!\2#\2%\2\'\2)\2+\2-\2/\2\61\2\63\2\65\2\67\29\2;\2=\2?\2A\2C\2E\2G"+
		"\2I\2K\2M\2\3\2\13\5\2\13\f\17\17\"\"\4\2\f\f\17\17\5\2C\\aac|\6\2\62"+
		";C\\aac|\4\2>>@@\4\2--//\3\2\62;\3\2\63;\5\2\f\f$$^^\u018c\2\3\3\2\2\2"+
		"\2\5\3\2\2\2\2\7\3\2\2\2\3P\3\2\2\2\5V\3\2\2\2\7a\3\2\2\2\to\3\2\2\2\13"+
		"v\3\2\2\2\rz\3\2\2\2\17|\3\2\2\2\21\u008b\3\2\2\2\23\u008d\3\2\2\2\25"+
		"\u0090\3\2\2\2\27\u0094\3\2\2\2\31\u009c\3\2\2\2\33\u00a1\3\2\2\2\35\u00a3"+
		"\3\2\2\2\37\u00b5\3\2\2\2!\u00b7\3\2\2\2#\u0102\3\2\2\2%\u0104\3\2\2\2"+
		"\'\u010d\3\2\2\2)\u010f\3\2\2\2+\u0111\3\2\2\2-\u011a\3\2\2\2/\u0123\3"+
		"\2\2\2\61\u0127\3\2\2\2\63\u012b\3\2\2\2\65\u013c\3\2\2\2\67\u013e\3\2"+
		"\2\29\u0141\3\2\2\2;\u0145\3\2\2\2=\u0153\3\2\2\2?\u0155\3\2\2\2A\u0160"+
		"\3\2\2\2C\u016a\3\2\2\2E\u016c\3\2\2\2G\u016e\3\2\2\2I\u0170\3\2\2\2K"+
		"\u0179\3\2\2\2M\u0184\3\2\2\2OQ\t\2\2\2PO\3\2\2\2QR\3\2\2\2RP\3\2\2\2"+
		"RS\3\2\2\2ST\3\2\2\2TU\b\2\2\2U\4\3\2\2\2VW\7\61\2\2WX\7\61\2\2X\\\3\2"+
		"\2\2Y[\n\3\2\2ZY\3\2\2\2[^\3\2\2\2\\Z\3\2\2\2\\]\3\2\2\2]_\3\2\2\2^\\"+
		"\3\2\2\2_`\b\3\2\2`\6\3\2\2\2ab\7\61\2\2bc\7,\2\2cg\3\2\2\2df\13\2\2\2"+
		"ed\3\2\2\2fi\3\2\2\2gh\3\2\2\2ge\3\2\2\2hj\3\2\2\2ig\3\2\2\2jk\7,\2\2"+
		"kl\7\61\2\2lm\3\2\2\2mn\b\4\2\2n\b\3\2\2\2os\t\4\2\2pr\5\13\6\2qp\3\2"+
		"\2\2ru\3\2\2\2sq\3\2\2\2st\3\2\2\2t\n\3\2\2\2us\3\2\2\2vw\t\5\2\2w\f\3"+
		"\2\2\2x{\5\17\b\2y{\5\25\13\2zx\3\2\2\2zy\3\2\2\2{\16\3\2\2\2|}\5\35\17"+
		"\2}~\5\t\5\2~\177\7*\2\2\177\u0080\5\21\t\2\u0080\u0081\7+\2\2\u0081\u0082"+
		"\5!\21\2\u0082\20\3\2\2\2\u0083\u0088\5\23\n\2\u0084\u0085\7.\2\2\u0085"+
		"\u0087\5\23\n\2\u0086\u0084\3\2\2\2\u0087\u008a\3\2\2\2\u0088\u0086\3"+
		"\2\2\2\u0088\u0089\3\2\2\2\u0089\u008c\3\2\2\2\u008a\u0088\3\2\2\2\u008b"+
		"\u0083\3\2\2\2\u008b\u008c\3\2\2\2\u008c\22\3\2\2\2\u008d\u008e\5\35\17"+
		"\2\u008e\u008f\5\t\5\2\u008f\24\3\2\2\2\u0090\u0091\5\35\17\2\u0091\u0092"+
		"\5\27\f\2\u0092\u0093\7=\2\2\u0093\26\3\2\2\2\u0094\u0099\5\31\r\2\u0095"+
		"\u0096\7.\2\2\u0096\u0098\5\31\r\2\u0097\u0095\3\2\2\2\u0098\u009b\3\2"+
		"\2\2\u0099\u0097\3\2\2\2\u0099\u009a\3\2\2\2\u009a\30\3\2\2\2\u009b\u0099"+
		"\3\2\2\2\u009c\u009f\5\t\5\2\u009d\u009e\7?\2\2\u009e\u00a0\5\33\16\2"+
		"\u009f\u009d\3\2\2\2\u009f\u00a0\3\2\2\2\u00a0\32\3\2\2\2\u00a1\u00a2"+
		"\5%\23\2\u00a2\34\3\2\2\2\u00a3\u00a4\5\37\20\2\u00a4\36\3\2\2\2\u00a5"+
		"\u00a6\7k\2\2\u00a6\u00a7\7p\2\2\u00a7\u00b6\7v\2\2\u00a8\u00a9\7u\2\2"+
		"\u00a9\u00aa\7v\2\2\u00aa\u00ab\7t\2\2\u00ab\u00ac\7k\2\2\u00ac\u00ad"+
		"\7p\2\2\u00ad\u00b6\7i\2\2\u00ae\u00af\7d\2\2\u00af\u00b0\7q\2\2\u00b0"+
		"\u00b1\7q\2\2\u00b1\u00b2\7n\2\2\u00b2\u00b3\7g\2\2\u00b3\u00b4\7c\2\2"+
		"\u00b4\u00b6\7p\2\2\u00b5\u00a5\3\2\2\2\u00b5\u00a8\3\2\2\2\u00b5\u00ae"+
		"\3\2\2\2\u00b6 \3\2\2\2\u00b7\u00bc\7}\2\2\u00b8\u00bb\5#\22\2\u00b9\u00bb"+
		"\5\r\7\2\u00ba\u00b8\3\2\2\2\u00ba\u00b9\3\2\2\2\u00bb\u00be\3\2\2\2\u00bc"+
		"\u00ba\3\2\2\2\u00bc\u00bd\3\2\2\2\u00bd\u00bf\3\2\2\2\u00be\u00bc\3\2"+
		"\2\2\u00bf\u00c0\7\177\2\2\u00c0\"\3\2\2\2\u00c1\u0103\5!\21\2\u00c2\u00c3"+
		"\7k\2\2\u00c3\u00c4\7h\2\2\u00c4\u00c5\3\2\2\2\u00c5\u00c6\7*\2\2\u00c6"+
		"\u00c7\5%\23\2\u00c7\u00c8\7+\2\2\u00c8\u00cf\5!\21\2\u00c9\u00ca\7g\2"+
		"\2\u00ca\u00cb\7n\2\2\u00cb\u00cc\7u\2\2\u00cc\u00cd\7g\2\2\u00cd\u00ce"+
		"\3\2\2\2\u00ce\u00d0\5!\21\2\u00cf\u00c9\3\2\2\2\u00cf\u00d0\3\2\2\2\u00d0"+
		"\u0103\3\2\2\2\u00d1\u00d2\7t\2\2\u00d2\u00d3\7g\2\2\u00d3\u00d4\7v\2"+
		"\2\u00d4\u00d5\7w\2\2\u00d5\u00d6\7t\2\2\u00d6\u00d7\7p\2\2\u00d7\u00d9"+
		"\3\2\2\2\u00d8\u00da\5%\23\2\u00d9\u00d8\3\2\2\2\u00d9\u00da\3\2\2\2\u00da"+
		"\u00db\3\2\2\2\u00db\u0103\7=\2\2\u00dc\u00dd\7h\2\2\u00dd\u00de\7q\2"+
		"\2\u00de\u00df\7t\2\2\u00df\u00e0\3\2\2\2\u00e0\u00e2\7*\2\2\u00e1\u00e3"+
		"\5%\23\2\u00e2\u00e1\3\2\2\2\u00e2\u00e3\3\2\2\2\u00e3\u00e4\3\2\2\2\u00e4"+
		"\u00e6\7=\2\2\u00e5\u00e7\5%\23\2\u00e6\u00e5\3\2\2\2\u00e6\u00e7\3\2"+
		"\2\2\u00e7\u00e8\3\2\2\2\u00e8\u00ea\7=\2\2\u00e9\u00eb\5%\23\2\u00ea"+
		"\u00e9\3\2\2\2\u00ea\u00eb\3\2\2\2\u00eb\u00ec\3\2\2\2\u00ec\u00ed\7+"+
		"\2\2\u00ed\u0103\5!\21\2\u00ee\u00ef\7h\2\2\u00ef\u00f0\7q\2\2\u00f0\u00f1"+
		"\7t\2\2\u00f1\u00f2\3\2\2\2\u00f2\u00f3\7*\2\2\u00f3\u00f5\5\25\13\2\u00f4"+
		"\u00f6\5%\23\2\u00f5\u00f4\3\2\2\2\u00f5\u00f6\3\2\2\2\u00f6\u00f7\3\2"+
		"\2\2\u00f7\u00f9\7=\2\2\u00f8\u00fa\5%\23\2\u00f9\u00f8\3\2\2\2\u00f9"+
		"\u00fa\3\2\2\2\u00fa\u00fb\3\2\2\2\u00fb\u00fc\7+\2\2\u00fc\u00fd\5!\21"+
		"\2\u00fd\u0103\3\2\2\2\u00fe\u00ff\5%\23\2\u00ff\u0100\7=\2\2\u0100\u0103"+
		"\3\2\2\2\u0101\u0103\7=\2\2\u0102\u00c1\3\2\2\2\u0102\u00c2\3\2\2\2\u0102"+
		"\u00d1\3\2\2\2\u0102\u00dc\3\2\2\2\u0102\u00ee\3\2\2\2\u0102\u00fe\3\2"+
		"\2\2\u0102\u0101\3\2\2\2\u0103$\3\2\2\2\u0104\u0105\5\'\24\2\u0105\u0106"+
		"\7.\2\2\u0106\u0107\5\'\24\2\u0107&\3\2\2\2\u0108\u0109\5\65\33\2\u0109"+
		"\u010a\7?\2\2\u010a\u010b\5\'\24\2\u010b\u010e\3\2\2\2\u010c\u010e\5+"+
		"\26\2\u010d\u0108\3\2\2\2\u010d\u010c\3\2\2\2\u010e(\3\2\2\2\u010f\u0110"+
		"\5+\26\2\u0110*\3\2\2\2\u0111\u0116\5-\27\2\u0112\u0113\7~\2\2\u0113\u0117"+
		"\7~\2\2\u0114\u0115\7(\2\2\u0115\u0117\7(\2\2\u0116\u0112\3\2\2\2\u0116"+
		"\u0114\3\2\2\2\u0117\u0118\3\2\2\2\u0118\u0119\5-\27\2\u0119,\3\2\2\2"+
		"\u011a\u011f\5/\30\2\u011b\u011c\7?\2\2\u011c\u0120\7?\2\2\u011d\u011e"+
		"\7#\2\2\u011e\u0120\7?\2\2\u011f\u011b\3\2\2\2\u011f\u011d\3\2\2\2\u0120"+
		"\u0121\3\2\2\2\u0121\u0122\5/\30\2\u0122.\3\2\2\2\u0123\u0124\5\61\31"+
		"\2\u0124\u0125\t\6\2\2\u0125\u0126\5\61\31\2\u0126\60\3\2\2\2\u0127\u0128"+
		"\5\63\32\2\u0128\u0129\t\7\2\2\u0129\u012a\5\63\32\2\u012a\62\3\2\2\2"+
		"\u012b\u012c\5\65\33\2\u012c\u012d\7,\2\2\u012d\u012e\7\61\2\2\u012e\u012f"+
		"\3\2\2\2\u012f\u0130\5\65\33\2\u0130\64\3\2\2\2\u0131\u013d\5\67\34\2"+
		"\u0132\u0133\7-\2\2\u0133\u0134\7-\2\2\u0134\u0135\3\2\2\2\u0135\u013d"+
		"\5\65\33\2\u0136\u0137\7/\2\2\u0137\u0138\7/\2\2\u0138\u0139\3\2\2\2\u0139"+
		"\u013d\5\65\33\2\u013a\u013b\7#\2\2\u013b\u013d\5\65\33\2\u013c\u0131"+
		"\3\2\2\2\u013c\u0132\3\2\2\2\u013c\u0136\3\2\2\2\u013c\u013a\3\2\2\2\u013d"+
		"\66\3\2\2\2\u013e\u013f\5=\37\2\u013f\u0140\59\35\2\u01408\3\2\2\2\u0141"+
		"\u0142\7*\2\2\u0142\u0143\5;\36\2\u0143\u0144\7+\2\2\u0144:\3\2\2\2\u0145"+
		"\u014a\5\'\24\2\u0146\u0147\7.\2\2\u0147\u0149\5\'\24\2\u0148\u0146\3"+
		"\2\2\2\u0149\u014c\3\2\2\2\u014a\u0148\3\2\2\2\u014a\u014b\3\2\2\2\u014b"+
		"<\3\2\2\2\u014c\u014a\3\2\2\2\u014d\u014e\7*\2\2\u014e\u014f\5%\23\2\u014f"+
		"\u0150\7+\2\2\u0150\u0154\3\2\2\2\u0151\u0154\5A!\2\u0152\u0154\5\t\5"+
		"\2\u0153\u014d\3\2\2\2\u0153\u0151\3\2\2\2\u0153\u0152\3\2\2\2\u0154>"+
		"\3\2\2\2\u0155\u015a\5%\23\2\u0156\u0157\7.\2\2\u0157\u0159\5%\23\2\u0158"+
		"\u0156\3\2\2\2\u0159\u015c\3\2\2\2\u015a\u0158\3\2\2\2\u015a\u015b\3\2"+
		"\2\2\u015b@\3\2\2\2\u015c\u015a\3\2\2\2\u015d\u0161\5C\"\2\u015e\u0161"+
		"\5I%\2\u015f\u0161\5M\'\2\u0160\u015d\3\2\2\2\u0160\u015e\3\2\2\2\u0160"+
		"\u015f\3\2\2\2\u0161B\3\2\2\2\u0162\u016b\7\62\2\2\u0163\u0167\5G$\2\u0164"+
		"\u0166\5E#\2\u0165\u0164\3\2\2\2\u0166\u0169\3\2\2\2\u0167\u0165\3\2\2"+
		"\2\u0167\u0168\3\2\2\2\u0168\u016b\3\2\2\2\u0169\u0167\3\2\2\2\u016a\u0162"+
		"\3\2\2\2\u016a\u0163\3\2\2\2\u016bD\3\2\2\2\u016c\u016d\t\b\2\2\u016d"+
		"F\3\2\2\2\u016e\u016f\t\t\2\2\u016fH\3\2\2\2\u0170\u0174\7$\2\2\u0171"+
		"\u0173\5K&\2\u0172\u0171\3\2\2\2\u0173\u0176\3\2\2\2\u0174\u0172\3\2\2"+
		"\2\u0174\u0175\3\2\2\2\u0175\u0177\3\2\2\2\u0176\u0174\3\2\2\2\u0177\u0178"+
		"\7$\2\2\u0178J\3\2\2\2\u0179\u017a\n\n\2\2\u017aL\3\2\2\2\u017b\u017c"+
		"\7v\2\2\u017c\u017d\7t\2\2\u017d\u017e\7w\2\2\u017e\u0185\7g\2\2\u017f"+
		"\u0180\7h\2\2\u0180\u0181\7c\2\2\u0181\u0182\7n\2\2\u0182\u0183\7u\2\2"+
		"\u0183\u0185\7g\2\2\u0184\u017b\3\2\2\2\u0184\u017f\3\2\2\2\u0185N\3\2"+
		"\2\2#\2R\\gsz\u0088\u008b\u0099\u009f\u00b5\u00ba\u00bc\u00cf\u00d9\u00e2"+
		"\u00e6\u00ea\u00f5\u00f9\u0102\u010d\u0116\u011f\u013c\u014a\u0153\u015a"+
		"\u0160\u0167\u016a\u0174\u0184\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}