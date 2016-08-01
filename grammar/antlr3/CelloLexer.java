// $ANTLR 3.5.2 Cello.g 2016-08-01 17:02:04

import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;

@SuppressWarnings("all")
public class CelloLexer extends Lexer {
	public static final int EOF=-1;
	public static final int T__14=14;
	public static final int T__15=15;
	public static final int T__16=16;
	public static final int T__17=17;
	public static final int T__18=18;
	public static final int T__19=19;
	public static final int T__20=20;
	public static final int T__21=21;
	public static final int T__22=22;
	public static final int T__23=23;
	public static final int T__24=24;
	public static final int T__25=25;
	public static final int T__26=26;
	public static final int T__27=27;
	public static final int T__28=28;
	public static final int T__29=29;
	public static final int T__30=30;
	public static final int T__31=31;
	public static final int T__32=32;
	public static final int T__33=33;
	public static final int T__34=34;
	public static final int T__35=35;
	public static final int T__36=36;
	public static final int T__37=37;
	public static final int T__38=38;
	public static final int T__39=39;
	public static final int T__40=40;
	public static final int T__41=41;
	public static final int T__42=42;
	public static final int BlockComment=4;
	public static final int DIGIT=5;
	public static final int IntegerLiteral=6;
	public static final int LineComment=7;
	public static final int NAME=8;
	public static final int NONZERODIGIT=9;
	public static final int STRING_CONTENT=10;
	public static final int StringLiteral=11;
	public static final int W=12;
	public static final int WhiteSpace=13;

	// delegates
	// delegators
	public Lexer[] getDelegates() {
		return new Lexer[] {};
	}

	public CelloLexer() {} 
	public CelloLexer(CharStream input) {
		this(input, new RecognizerSharedState());
	}
	public CelloLexer(CharStream input, RecognizerSharedState state) {
		super(input,state);
	}
	@Override public String getGrammarFileName() { return "Cello.g"; }

	// $ANTLR start "T__14"
	public final void mT__14() throws RecognitionException {
		try {
			int _type = T__14;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:2:7: ( '!' )
			// Cello.g:2:9: '!'
			{
			match('!'); 
			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__14"

	// $ANTLR start "T__15"
	public final void mT__15() throws RecognitionException {
		try {
			int _type = T__15;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:3:7: ( '!=' )
			// Cello.g:3:9: '!='
			{
			match("!="); 

			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__15"

	// $ANTLR start "T__16"
	public final void mT__16() throws RecognitionException {
		try {
			int _type = T__16;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:4:7: ( '&&' )
			// Cello.g:4:9: '&&'
			{
			match("&&"); 

			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__16"

	// $ANTLR start "T__17"
	public final void mT__17() throws RecognitionException {
		try {
			int _type = T__17;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:5:7: ( '(' )
			// Cello.g:5:9: '('
			{
			match('('); 
			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__17"

	// $ANTLR start "T__18"
	public final void mT__18() throws RecognitionException {
		try {
			int _type = T__18;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:6:7: ( ')' )
			// Cello.g:6:9: ')'
			{
			match(')'); 
			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__18"

	// $ANTLR start "T__19"
	public final void mT__19() throws RecognitionException {
		try {
			int _type = T__19;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:7:7: ( '*' )
			// Cello.g:7:9: '*'
			{
			match('*'); 
			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__19"

	// $ANTLR start "T__20"
	public final void mT__20() throws RecognitionException {
		try {
			int _type = T__20;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:8:7: ( '+' )
			// Cello.g:8:9: '+'
			{
			match('+'); 
			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__20"

	// $ANTLR start "T__21"
	public final void mT__21() throws RecognitionException {
		try {
			int _type = T__21;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:9:7: ( '++' )
			// Cello.g:9:9: '++'
			{
			match("++"); 

			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__21"

	// $ANTLR start "T__22"
	public final void mT__22() throws RecognitionException {
		try {
			int _type = T__22;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:10:7: ( ',' )
			// Cello.g:10:9: ','
			{
			match(','); 
			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__22"

	// $ANTLR start "T__23"
	public final void mT__23() throws RecognitionException {
		try {
			int _type = T__23;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:11:7: ( '-' )
			// Cello.g:11:9: '-'
			{
			match('-'); 
			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__23"

	// $ANTLR start "T__24"
	public final void mT__24() throws RecognitionException {
		try {
			int _type = T__24;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:12:7: ( '--' )
			// Cello.g:12:9: '--'
			{
			match("--"); 

			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__24"

	// $ANTLR start "T__25"
	public final void mT__25() throws RecognitionException {
		try {
			int _type = T__25;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:13:7: ( '/' )
			// Cello.g:13:9: '/'
			{
			match('/'); 
			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__25"

	// $ANTLR start "T__26"
	public final void mT__26() throws RecognitionException {
		try {
			int _type = T__26;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:14:7: ( ';' )
			// Cello.g:14:9: ';'
			{
			match(';'); 
			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__26"

	// $ANTLR start "T__27"
	public final void mT__27() throws RecognitionException {
		try {
			int _type = T__27;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:15:7: ( '<' )
			// Cello.g:15:9: '<'
			{
			match('<'); 
			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__27"

	// $ANTLR start "T__28"
	public final void mT__28() throws RecognitionException {
		try {
			int _type = T__28;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:16:7: ( '=' )
			// Cello.g:16:9: '='
			{
			match('='); 
			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__28"

	// $ANTLR start "T__29"
	public final void mT__29() throws RecognitionException {
		try {
			int _type = T__29;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:17:7: ( '==' )
			// Cello.g:17:9: '=='
			{
			match("=="); 

			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__29"

	// $ANTLR start "T__30"
	public final void mT__30() throws RecognitionException {
		try {
			int _type = T__30;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:18:7: ( '>' )
			// Cello.g:18:9: '>'
			{
			match('>'); 
			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__30"

	// $ANTLR start "T__31"
	public final void mT__31() throws RecognitionException {
		try {
			int _type = T__31;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:19:7: ( 'boolean' )
			// Cello.g:19:9: 'boolean'
			{
			match("boolean"); 

			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__31"

	// $ANTLR start "T__32"
	public final void mT__32() throws RecognitionException {
		try {
			int _type = T__32;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:20:7: ( 'else' )
			// Cello.g:20:9: 'else'
			{
			match("else"); 

			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__32"

	// $ANTLR start "T__33"
	public final void mT__33() throws RecognitionException {
		try {
			int _type = T__33;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:21:7: ( 'false' )
			// Cello.g:21:9: 'false'
			{
			match("false"); 

			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__33"

	// $ANTLR start "T__34"
	public final void mT__34() throws RecognitionException {
		try {
			int _type = T__34;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:22:7: ( 'for' )
			// Cello.g:22:9: 'for'
			{
			match("for"); 

			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__34"

	// $ANTLR start "T__35"
	public final void mT__35() throws RecognitionException {
		try {
			int _type = T__35;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:23:7: ( 'if' )
			// Cello.g:23:9: 'if'
			{
			match("if"); 

			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__35"

	// $ANTLR start "T__36"
	public final void mT__36() throws RecognitionException {
		try {
			int _type = T__36;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:24:7: ( 'int' )
			// Cello.g:24:9: 'int'
			{
			match("int"); 

			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__36"

	// $ANTLR start "T__37"
	public final void mT__37() throws RecognitionException {
		try {
			int _type = T__37;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:25:7: ( 'return' )
			// Cello.g:25:9: 'return'
			{
			match("return"); 

			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__37"

	// $ANTLR start "T__38"
	public final void mT__38() throws RecognitionException {
		try {
			int _type = T__38;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:26:7: ( 'string' )
			// Cello.g:26:9: 'string'
			{
			match("string"); 

			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__38"

	// $ANTLR start "T__39"
	public final void mT__39() throws RecognitionException {
		try {
			int _type = T__39;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:27:7: ( 'true' )
			// Cello.g:27:9: 'true'
			{
			match("true"); 

			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__39"

	// $ANTLR start "T__40"
	public final void mT__40() throws RecognitionException {
		try {
			int _type = T__40;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:28:7: ( '{' )
			// Cello.g:28:9: '{'
			{
			match('{'); 
			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__40"

	// $ANTLR start "T__41"
	public final void mT__41() throws RecognitionException {
		try {
			int _type = T__41;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:29:7: ( '||' )
			// Cello.g:29:9: '||'
			{
			match("||"); 

			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__41"

	// $ANTLR start "T__42"
	public final void mT__42() throws RecognitionException {
		try {
			int _type = T__42;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:30:7: ( '}' )
			// Cello.g:30:9: '}'
			{
			match('}'); 
			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "T__42"

	// $ANTLR start "WhiteSpace"
	public final void mWhiteSpace() throws RecognitionException {
		try {
			int _type = WhiteSpace;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:8:5: ( ( ' ' | '\\r' | '\\t' | '\\n' ) )
			// Cello.g:8:9: ( ' ' | '\\r' | '\\t' | '\\n' )
			{
			if ( (input.LA(1) >= '\t' && input.LA(1) <= '\n')||input.LA(1)=='\r'||input.LA(1)==' ' ) {
				input.consume();
			}
			else {
				MismatchedSetException mse = new MismatchedSetException(null,input);
				recover(mse);
				throw mse;
			}

			                skip();
			            
			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "WhiteSpace"

	// $ANTLR start "BlockComment"
	public final void mBlockComment() throws RecognitionException {
		try {
			int _type = BlockComment;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:18:13: ( '/*' ( options {greedy=false; } : . )* '*/' )
			// Cello.g:18:17: '/*' ( options {greedy=false; } : . )* '*/'
			{
			match("/*"); 

			// Cello.g:18:22: ( options {greedy=false; } : . )*
			loop1:
			while (true) {
				int alt1=2;
				int LA1_0 = input.LA(1);
				if ( (LA1_0=='*') ) {
					int LA1_1 = input.LA(2);
					if ( (LA1_1=='/') ) {
						alt1=2;
					}
					else if ( ((LA1_1 >= '\u0000' && LA1_1 <= '.')||(LA1_1 >= '0' && LA1_1 <= '\uFFFF')) ) {
						alt1=1;
					}

				}
				else if ( ((LA1_0 >= '\u0000' && LA1_0 <= ')')||(LA1_0 >= '+' && LA1_0 <= '\uFFFF')) ) {
					alt1=1;
				}

				switch (alt1) {
				case 1 :
					// Cello.g:18:49: .
					{
					matchAny(); 
					}
					break;

				default :
					break loop1;
				}
			}

			match("*/"); 


			                skip();
			            
			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "BlockComment"

	// $ANTLR start "LineComment"
	public final void mLineComment() throws RecognitionException {
		try {
			int _type = LineComment;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:25:5: ( '//' (~ ( '\\n' | '\\r' ) )* ( '\\r\\n' | '\\r' | '\\n' ) | '//' (~ ( '\\n' | '\\r' ) )* )
			int alt5=2;
			alt5 = dfa5.predict(input);
			switch (alt5) {
				case 1 :
					// Cello.g:25:9: '//' (~ ( '\\n' | '\\r' ) )* ( '\\r\\n' | '\\r' | '\\n' )
					{
					match("//"); 

					// Cello.g:25:14: (~ ( '\\n' | '\\r' ) )*
					loop2:
					while (true) {
						int alt2=2;
						int LA2_0 = input.LA(1);
						if ( ((LA2_0 >= '\u0000' && LA2_0 <= '\t')||(LA2_0 >= '\u000B' && LA2_0 <= '\f')||(LA2_0 >= '\u000E' && LA2_0 <= '\uFFFF')) ) {
							alt2=1;
						}

						switch (alt2) {
						case 1 :
							// Cello.g:
							{
							if ( (input.LA(1) >= '\u0000' && input.LA(1) <= '\t')||(input.LA(1) >= '\u000B' && input.LA(1) <= '\f')||(input.LA(1) >= '\u000E' && input.LA(1) <= '\uFFFF') ) {
								input.consume();
							}
							else {
								MismatchedSetException mse = new MismatchedSetException(null,input);
								recover(mse);
								throw mse;
							}
							}
							break;

						default :
							break loop2;
						}
					}

					// Cello.g:25:29: ( '\\r\\n' | '\\r' | '\\n' )
					int alt3=3;
					int LA3_0 = input.LA(1);
					if ( (LA3_0=='\r') ) {
						int LA3_1 = input.LA(2);
						if ( (LA3_1=='\n') ) {
							alt3=1;
						}

						else {
							alt3=2;
						}

					}
					else if ( (LA3_0=='\n') ) {
						alt3=3;
					}

					else {
						NoViableAltException nvae =
							new NoViableAltException("", 3, 0, input);
						throw nvae;
					}

					switch (alt3) {
						case 1 :
							// Cello.g:25:30: '\\r\\n'
							{
							match("\r\n"); 

							}
							break;
						case 2 :
							// Cello.g:25:39: '\\r'
							{
							match('\r'); 
							}
							break;
						case 3 :
							// Cello.g:25:46: '\\n'
							{
							match('\n'); 
							}
							break;

					}


					                skip();
					            
					}
					break;
				case 2 :
					// Cello.g:29:9: '//' (~ ( '\\n' | '\\r' ) )*
					{
					match("//"); 

					// Cello.g:29:14: (~ ( '\\n' | '\\r' ) )*
					loop4:
					while (true) {
						int alt4=2;
						int LA4_0 = input.LA(1);
						if ( ((LA4_0 >= '\u0000' && LA4_0 <= '\t')||(LA4_0 >= '\u000B' && LA4_0 <= '\f')||(LA4_0 >= '\u000E' && LA4_0 <= '\uFFFF')) ) {
							alt4=1;
						}

						switch (alt4) {
						case 1 :
							// Cello.g:
							{
							if ( (input.LA(1) >= '\u0000' && input.LA(1) <= '\t')||(input.LA(1) >= '\u000B' && input.LA(1) <= '\f')||(input.LA(1) >= '\u000E' && input.LA(1) <= '\uFFFF') ) {
								input.consume();
							}
							else {
								MismatchedSetException mse = new MismatchedSetException(null,input);
								recover(mse);
								throw mse;
							}
							}
							break;

						default :
							break loop4;
						}
					}


					                skip();
					            
					}
					break;

			}
			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "LineComment"

	// $ANTLR start "NAME"
	public final void mNAME() throws RecognitionException {
		try {
			int _type = NAME;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:35:5: ( ( 'a' .. 'z' | 'A' .. 'Z' | '_' ) ( W )* )
			// Cello.g:35:6: ( 'a' .. 'z' | 'A' .. 'Z' | '_' ) ( W )*
			{
			if ( (input.LA(1) >= 'A' && input.LA(1) <= 'Z')||input.LA(1)=='_'||(input.LA(1) >= 'a' && input.LA(1) <= 'z') ) {
				input.consume();
			}
			else {
				MismatchedSetException mse = new MismatchedSetException(null,input);
				recover(mse);
				throw mse;
			}
			// Cello.g:35:30: ( W )*
			loop6:
			while (true) {
				int alt6=2;
				int LA6_0 = input.LA(1);
				if ( ((LA6_0 >= '0' && LA6_0 <= '9')||(LA6_0 >= 'A' && LA6_0 <= 'Z')||LA6_0=='_'||(LA6_0 >= 'a' && LA6_0 <= 'z')) ) {
					alt6=1;
				}

				switch (alt6) {
				case 1 :
					// Cello.g:
					{
					if ( (input.LA(1) >= '0' && input.LA(1) <= '9')||(input.LA(1) >= 'A' && input.LA(1) <= 'Z')||input.LA(1)=='_'||(input.LA(1) >= 'a' && input.LA(1) <= 'z') ) {
						input.consume();
					}
					else {
						MismatchedSetException mse = new MismatchedSetException(null,input);
						recover(mse);
						throw mse;
					}
					}
					break;

				default :
					break loop6;
				}
			}

			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "NAME"

	// $ANTLR start "W"
	public final void mW() throws RecognitionException {
		try {
			// Cello.g:36:11: ( ( 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' ) )
			// Cello.g:
			{
			if ( (input.LA(1) >= '0' && input.LA(1) <= '9')||(input.LA(1) >= 'A' && input.LA(1) <= 'Z')||input.LA(1)=='_'||(input.LA(1) >= 'a' && input.LA(1) <= 'z') ) {
				input.consume();
			}
			else {
				MismatchedSetException mse = new MismatchedSetException(null,input);
				recover(mse);
				throw mse;
			}
			}

		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "W"

	// $ANTLR start "IntegerLiteral"
	public final void mIntegerLiteral() throws RecognitionException {
		try {
			int _type = IntegerLiteral;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:111:15: ( '0' | NONZERODIGIT ( DIGIT )* )
			int alt8=2;
			int LA8_0 = input.LA(1);
			if ( (LA8_0=='0') ) {
				alt8=1;
			}
			else if ( ((LA8_0 >= '1' && LA8_0 <= '9')) ) {
				alt8=2;
			}

			else {
				NoViableAltException nvae =
					new NoViableAltException("", 8, 0, input);
				throw nvae;
			}

			switch (alt8) {
				case 1 :
					// Cello.g:111:17: '0'
					{
					match('0'); 
					}
					break;
				case 2 :
					// Cello.g:112:17: NONZERODIGIT ( DIGIT )*
					{
					mNONZERODIGIT(); 

					// Cello.g:112:30: ( DIGIT )*
					loop7:
					while (true) {
						int alt7=2;
						int LA7_0 = input.LA(1);
						if ( ((LA7_0 >= '0' && LA7_0 <= '9')) ) {
							alt7=1;
						}

						switch (alt7) {
						case 1 :
							// Cello.g:
							{
							if ( (input.LA(1) >= '0' && input.LA(1) <= '9') ) {
								input.consume();
							}
							else {
								MismatchedSetException mse = new MismatchedSetException(null,input);
								recover(mse);
								throw mse;
							}
							}
							break;

						default :
							break loop7;
						}
					}

					}
					break;

			}
			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "IntegerLiteral"

	// $ANTLR start "DIGIT"
	public final void mDIGIT() throws RecognitionException {
		try {
			// Cello.g:114:15: ( '0' .. '9' )
			// Cello.g:
			{
			if ( (input.LA(1) >= '0' && input.LA(1) <= '9') ) {
				input.consume();
			}
			else {
				MismatchedSetException mse = new MismatchedSetException(null,input);
				recover(mse);
				throw mse;
			}
			}

		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "DIGIT"

	// $ANTLR start "NONZERODIGIT"
	public final void mNONZERODIGIT() throws RecognitionException {
		try {
			// Cello.g:115:22: ( '1' .. '9' )
			// Cello.g:
			{
			if ( (input.LA(1) >= '1' && input.LA(1) <= '9') ) {
				input.consume();
			}
			else {
				MismatchedSetException mse = new MismatchedSetException(null,input);
				recover(mse);
				throw mse;
			}
			}

		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "NONZERODIGIT"

	// $ANTLR start "StringLiteral"
	public final void mStringLiteral() throws RecognitionException {
		try {
			int _type = StringLiteral;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// Cello.g:117:14: ( '\"' ( STRING_CONTENT )* '\"' )
			// Cello.g:117:16: '\"' ( STRING_CONTENT )* '\"'
			{
			match('\"'); 
			// Cello.g:117:20: ( STRING_CONTENT )*
			loop9:
			while (true) {
				int alt9=2;
				int LA9_0 = input.LA(1);
				if ( ((LA9_0 >= '\u0000' && LA9_0 <= '\t')||(LA9_0 >= '\u000B' && LA9_0 <= '\f')||(LA9_0 >= '\u000E' && LA9_0 <= '!')||(LA9_0 >= '#' && LA9_0 <= '[')||(LA9_0 >= ']' && LA9_0 <= '\uFFFF')) ) {
					alt9=1;
				}

				switch (alt9) {
				case 1 :
					// Cello.g:
					{
					if ( (input.LA(1) >= '\u0000' && input.LA(1) <= '\t')||(input.LA(1) >= '\u000B' && input.LA(1) <= '\f')||(input.LA(1) >= '\u000E' && input.LA(1) <= '!')||(input.LA(1) >= '#' && input.LA(1) <= '[')||(input.LA(1) >= ']' && input.LA(1) <= '\uFFFF') ) {
						input.consume();
					}
					else {
						MismatchedSetException mse = new MismatchedSetException(null,input);
						recover(mse);
						throw mse;
					}
					}
					break;

				default :
					break loop9;
				}
			}

			match('\"'); 
			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "StringLiteral"

	// $ANTLR start "STRING_CONTENT"
	public final void mSTRING_CONTENT() throws RecognitionException {
		try {
			// Cello.g:118:24: (~ ( '\\\\' | '\"' | '\\r' | '\\n' ) )
			// Cello.g:
			{
			if ( (input.LA(1) >= '\u0000' && input.LA(1) <= '\t')||(input.LA(1) >= '\u000B' && input.LA(1) <= '\f')||(input.LA(1) >= '\u000E' && input.LA(1) <= '!')||(input.LA(1) >= '#' && input.LA(1) <= '[')||(input.LA(1) >= ']' && input.LA(1) <= '\uFFFF') ) {
				input.consume();
			}
			else {
				MismatchedSetException mse = new MismatchedSetException(null,input);
				recover(mse);
				throw mse;
			}
			}

		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "STRING_CONTENT"

	@Override
	public void mTokens() throws RecognitionException {
		// Cello.g:1:8: ( T__14 | T__15 | T__16 | T__17 | T__18 | T__19 | T__20 | T__21 | T__22 | T__23 | T__24 | T__25 | T__26 | T__27 | T__28 | T__29 | T__30 | T__31 | T__32 | T__33 | T__34 | T__35 | T__36 | T__37 | T__38 | T__39 | T__40 | T__41 | T__42 | WhiteSpace | BlockComment | LineComment | NAME | IntegerLiteral | StringLiteral )
		int alt10=35;
		alt10 = dfa10.predict(input);
		switch (alt10) {
			case 1 :
				// Cello.g:1:10: T__14
				{
				mT__14(); 

				}
				break;
			case 2 :
				// Cello.g:1:16: T__15
				{
				mT__15(); 

				}
				break;
			case 3 :
				// Cello.g:1:22: T__16
				{
				mT__16(); 

				}
				break;
			case 4 :
				// Cello.g:1:28: T__17
				{
				mT__17(); 

				}
				break;
			case 5 :
				// Cello.g:1:34: T__18
				{
				mT__18(); 

				}
				break;
			case 6 :
				// Cello.g:1:40: T__19
				{
				mT__19(); 

				}
				break;
			case 7 :
				// Cello.g:1:46: T__20
				{
				mT__20(); 

				}
				break;
			case 8 :
				// Cello.g:1:52: T__21
				{
				mT__21(); 

				}
				break;
			case 9 :
				// Cello.g:1:58: T__22
				{
				mT__22(); 

				}
				break;
			case 10 :
				// Cello.g:1:64: T__23
				{
				mT__23(); 

				}
				break;
			case 11 :
				// Cello.g:1:70: T__24
				{
				mT__24(); 

				}
				break;
			case 12 :
				// Cello.g:1:76: T__25
				{
				mT__25(); 

				}
				break;
			case 13 :
				// Cello.g:1:82: T__26
				{
				mT__26(); 

				}
				break;
			case 14 :
				// Cello.g:1:88: T__27
				{
				mT__27(); 

				}
				break;
			case 15 :
				// Cello.g:1:94: T__28
				{
				mT__28(); 

				}
				break;
			case 16 :
				// Cello.g:1:100: T__29
				{
				mT__29(); 

				}
				break;
			case 17 :
				// Cello.g:1:106: T__30
				{
				mT__30(); 

				}
				break;
			case 18 :
				// Cello.g:1:112: T__31
				{
				mT__31(); 

				}
				break;
			case 19 :
				// Cello.g:1:118: T__32
				{
				mT__32(); 

				}
				break;
			case 20 :
				// Cello.g:1:124: T__33
				{
				mT__33(); 

				}
				break;
			case 21 :
				// Cello.g:1:130: T__34
				{
				mT__34(); 

				}
				break;
			case 22 :
				// Cello.g:1:136: T__35
				{
				mT__35(); 

				}
				break;
			case 23 :
				// Cello.g:1:142: T__36
				{
				mT__36(); 

				}
				break;
			case 24 :
				// Cello.g:1:148: T__37
				{
				mT__37(); 

				}
				break;
			case 25 :
				// Cello.g:1:154: T__38
				{
				mT__38(); 

				}
				break;
			case 26 :
				// Cello.g:1:160: T__39
				{
				mT__39(); 

				}
				break;
			case 27 :
				// Cello.g:1:166: T__40
				{
				mT__40(); 

				}
				break;
			case 28 :
				// Cello.g:1:172: T__41
				{
				mT__41(); 

				}
				break;
			case 29 :
				// Cello.g:1:178: T__42
				{
				mT__42(); 

				}
				break;
			case 30 :
				// Cello.g:1:184: WhiteSpace
				{
				mWhiteSpace(); 

				}
				break;
			case 31 :
				// Cello.g:1:195: BlockComment
				{
				mBlockComment(); 

				}
				break;
			case 32 :
				// Cello.g:1:208: LineComment
				{
				mLineComment(); 

				}
				break;
			case 33 :
				// Cello.g:1:220: NAME
				{
				mNAME(); 

				}
				break;
			case 34 :
				// Cello.g:1:225: IntegerLiteral
				{
				mIntegerLiteral(); 

				}
				break;
			case 35 :
				// Cello.g:1:240: StringLiteral
				{
				mStringLiteral(); 

				}
				break;

		}
	}


	protected DFA5 dfa5 = new DFA5(this);
	protected DFA10 dfa10 = new DFA10(this);
	static final String DFA5_eotS =
		"\2\uffff\2\5\2\uffff";
	static final String DFA5_eofS =
		"\6\uffff";
	static final String DFA5_minS =
		"\2\57\2\0\2\uffff";
	static final String DFA5_maxS =
		"\2\57\2\uffff\2\uffff";
	static final String DFA5_acceptS =
		"\4\uffff\1\1\1\2";
	static final String DFA5_specialS =
		"\2\uffff\1\0\1\1\2\uffff}>";
	static final String[] DFA5_transitionS = {
			"\1\1",
			"\1\2",
			"\12\3\1\4\2\3\1\4\ufff2\3",
			"\12\3\1\4\2\3\1\4\ufff2\3",
			"",
			""
	};

	static final short[] DFA5_eot = DFA.unpackEncodedString(DFA5_eotS);
	static final short[] DFA5_eof = DFA.unpackEncodedString(DFA5_eofS);
	static final char[] DFA5_min = DFA.unpackEncodedStringToUnsignedChars(DFA5_minS);
	static final char[] DFA5_max = DFA.unpackEncodedStringToUnsignedChars(DFA5_maxS);
	static final short[] DFA5_accept = DFA.unpackEncodedString(DFA5_acceptS);
	static final short[] DFA5_special = DFA.unpackEncodedString(DFA5_specialS);
	static final short[][] DFA5_transition;

	static {
		int numStates = DFA5_transitionS.length;
		DFA5_transition = new short[numStates][];
		for (int i=0; i<numStates; i++) {
			DFA5_transition[i] = DFA.unpackEncodedString(DFA5_transitionS[i]);
		}
	}

	protected class DFA5 extends DFA {

		public DFA5(BaseRecognizer recognizer) {
			this.recognizer = recognizer;
			this.decisionNumber = 5;
			this.eot = DFA5_eot;
			this.eof = DFA5_eof;
			this.min = DFA5_min;
			this.max = DFA5_max;
			this.accept = DFA5_accept;
			this.special = DFA5_special;
			this.transition = DFA5_transition;
		}
		@Override
		public String getDescription() {
			return "24:1: LineComment : ( '//' (~ ( '\\n' | '\\r' ) )* ( '\\r\\n' | '\\r' | '\\n' ) | '//' (~ ( '\\n' | '\\r' ) )* );";
		}
		@Override
		public int specialStateTransition(int s, IntStream _input) throws NoViableAltException {
			IntStream input = _input;
			int _s = s;
			switch ( s ) {
					case 0 : 
						int LA5_2 = input.LA(1);
						s = -1;
						if ( ((LA5_2 >= '\u0000' && LA5_2 <= '\t')||(LA5_2 >= '\u000B' && LA5_2 <= '\f')||(LA5_2 >= '\u000E' && LA5_2 <= '\uFFFF')) ) {s = 3;}
						else if ( (LA5_2=='\n'||LA5_2=='\r') ) {s = 4;}
						else s = 5;
						if ( s>=0 ) return s;
						break;

					case 1 : 
						int LA5_3 = input.LA(1);
						s = -1;
						if ( (LA5_3=='\n'||LA5_3=='\r') ) {s = 4;}
						else if ( ((LA5_3 >= '\u0000' && LA5_3 <= '\t')||(LA5_3 >= '\u000B' && LA5_3 <= '\f')||(LA5_3 >= '\u000E' && LA5_3 <= '\uFFFF')) ) {s = 3;}
						else s = 5;
						if ( s>=0 ) return s;
						break;
			}
			NoViableAltException nvae =
				new NoViableAltException(getDescription(), 5, _s, input);
			error(nvae);
			throw nvae;
		}
	}

	static final String DFA10_eotS =
		"\1\uffff\1\35\4\uffff\1\37\1\uffff\1\41\1\44\2\uffff\1\46\1\uffff\7\31"+
		"\22\uffff\4\31\1\64\7\31\1\74\1\uffff\1\75\4\31\1\102\1\31\2\uffff\2\31"+
		"\1\106\1\31\1\uffff\1\110\2\31\1\uffff\1\31\1\uffff\1\114\1\115\1\116"+
		"\3\uffff";
	static final String DFA10_eofS =
		"\117\uffff";
	static final String DFA10_minS =
		"\1\11\1\75\4\uffff\1\53\1\uffff\1\55\1\52\2\uffff\1\75\1\uffff\1\157\1"+
		"\154\1\141\1\146\1\145\1\164\1\162\22\uffff\1\157\1\163\1\154\1\162\1"+
		"\60\2\164\1\162\1\165\1\154\1\145\1\163\1\60\1\uffff\1\60\1\165\1\151"+
		"\2\145\1\60\1\145\2\uffff\1\162\1\156\1\60\1\141\1\uffff\1\60\1\156\1"+
		"\147\1\uffff\1\156\1\uffff\3\60\3\uffff";
	static final String DFA10_maxS =
		"\1\175\1\75\4\uffff\1\53\1\uffff\1\55\1\57\2\uffff\1\75\1\uffff\1\157"+
		"\1\154\1\157\1\156\1\145\1\164\1\162\22\uffff\1\157\1\163\1\154\1\162"+
		"\1\172\2\164\1\162\1\165\1\154\1\145\1\163\1\172\1\uffff\1\172\1\165\1"+
		"\151\2\145\1\172\1\145\2\uffff\1\162\1\156\1\172\1\141\1\uffff\1\172\1"+
		"\156\1\147\1\uffff\1\156\1\uffff\3\172\3\uffff";
	static final String DFA10_acceptS =
		"\2\uffff\1\3\1\4\1\5\1\6\1\uffff\1\11\2\uffff\1\15\1\16\1\uffff\1\21\7"+
		"\uffff\1\33\1\34\1\35\1\36\1\41\1\42\1\43\1\2\1\1\1\10\1\7\1\13\1\12\1"+
		"\37\1\40\1\14\1\20\1\17\15\uffff\1\26\7\uffff\1\25\1\27\4\uffff\1\23\3"+
		"\uffff\1\32\1\uffff\1\24\3\uffff\1\30\1\31\1\22";
	static final String DFA10_specialS =
		"\117\uffff}>";
	static final String[] DFA10_transitionS = {
			"\2\30\2\uffff\1\30\22\uffff\1\30\1\1\1\33\3\uffff\1\2\1\uffff\1\3\1\4"+
			"\1\5\1\6\1\7\1\10\1\uffff\1\11\12\32\1\uffff\1\12\1\13\1\14\1\15\2\uffff"+
			"\32\31\4\uffff\1\31\1\uffff\1\31\1\16\2\31\1\17\1\20\2\31\1\21\10\31"+
			"\1\22\1\23\1\24\6\31\1\25\1\26\1\27",
			"\1\34",
			"",
			"",
			"",
			"",
			"\1\36",
			"",
			"\1\40",
			"\1\42\4\uffff\1\43",
			"",
			"",
			"\1\45",
			"",
			"\1\47",
			"\1\50",
			"\1\51\15\uffff\1\52",
			"\1\53\7\uffff\1\54",
			"\1\55",
			"\1\56",
			"\1\57",
			"",
			"",
			"",
			"",
			"",
			"",
			"",
			"",
			"",
			"",
			"",
			"",
			"",
			"",
			"",
			"",
			"",
			"",
			"\1\60",
			"\1\61",
			"\1\62",
			"\1\63",
			"\12\31\7\uffff\32\31\4\uffff\1\31\1\uffff\32\31",
			"\1\65",
			"\1\66",
			"\1\67",
			"\1\70",
			"\1\71",
			"\1\72",
			"\1\73",
			"\12\31\7\uffff\32\31\4\uffff\1\31\1\uffff\32\31",
			"",
			"\12\31\7\uffff\32\31\4\uffff\1\31\1\uffff\32\31",
			"\1\76",
			"\1\77",
			"\1\100",
			"\1\101",
			"\12\31\7\uffff\32\31\4\uffff\1\31\1\uffff\32\31",
			"\1\103",
			"",
			"",
			"\1\104",
			"\1\105",
			"\12\31\7\uffff\32\31\4\uffff\1\31\1\uffff\32\31",
			"\1\107",
			"",
			"\12\31\7\uffff\32\31\4\uffff\1\31\1\uffff\32\31",
			"\1\111",
			"\1\112",
			"",
			"\1\113",
			"",
			"\12\31\7\uffff\32\31\4\uffff\1\31\1\uffff\32\31",
			"\12\31\7\uffff\32\31\4\uffff\1\31\1\uffff\32\31",
			"\12\31\7\uffff\32\31\4\uffff\1\31\1\uffff\32\31",
			"",
			"",
			""
	};

	static final short[] DFA10_eot = DFA.unpackEncodedString(DFA10_eotS);
	static final short[] DFA10_eof = DFA.unpackEncodedString(DFA10_eofS);
	static final char[] DFA10_min = DFA.unpackEncodedStringToUnsignedChars(DFA10_minS);
	static final char[] DFA10_max = DFA.unpackEncodedStringToUnsignedChars(DFA10_maxS);
	static final short[] DFA10_accept = DFA.unpackEncodedString(DFA10_acceptS);
	static final short[] DFA10_special = DFA.unpackEncodedString(DFA10_specialS);
	static final short[][] DFA10_transition;

	static {
		int numStates = DFA10_transitionS.length;
		DFA10_transition = new short[numStates][];
		for (int i=0; i<numStates; i++) {
			DFA10_transition[i] = DFA.unpackEncodedString(DFA10_transitionS[i]);
		}
	}

	protected class DFA10 extends DFA {

		public DFA10(BaseRecognizer recognizer) {
			this.recognizer = recognizer;
			this.decisionNumber = 10;
			this.eot = DFA10_eot;
			this.eof = DFA10_eof;
			this.min = DFA10_min;
			this.max = DFA10_max;
			this.accept = DFA10_accept;
			this.special = DFA10_special;
			this.transition = DFA10_transition;
		}
		@Override
		public String getDescription() {
			return "1:1: Tokens : ( T__14 | T__15 | T__16 | T__17 | T__18 | T__19 | T__20 | T__21 | T__22 | T__23 | T__24 | T__25 | T__26 | T__27 | T__28 | T__29 | T__30 | T__31 | T__32 | T__33 | T__34 | T__35 | T__36 | T__37 | T__38 | T__39 | T__40 | T__41 | T__42 | WhiteSpace | BlockComment | LineComment | NAME | IntegerLiteral | StringLiteral );";
		}
	}

}
