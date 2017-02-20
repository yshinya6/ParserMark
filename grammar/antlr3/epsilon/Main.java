import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.debug.BlankDebugEventListener;

/**
 * Parse a java file or directory of java files using the generated parser ANTLR
 * builds from java.g
 */
class Main {
	public static long lexerTime = 0;
	public static boolean profile = false;
	public static boolean failed = false;
	static EpsilonLexer lexer;

	public static void main(String[] args) {
		try {
			if (args.length > 0) {
				for (int i = 0; i < args.length; i++) {
					parseFile(args[i]);
				}
			}
			else {
				System.err.println("Usage: java Main <directory or file name>");
			}

		} catch (Exception e) {

		}
	}
	// Here's where we do the real work...
	public static void parseFile(String f) throws Exception {
		long fastest = 0;
		try {
			for (int i = 0; i < 10; i++) {
				if (lexer == null) {
					lexer = new EpsilonLexer();
				}

				lexer.setCharStream(new ANTLRFileStream(f));
				CommonTokenStream tokens = new CommonTokenStream(lexer);
				long start = System.nanoTime();
				tokens.fill(); // force load
				EpsilonParser parser = new EpsilonParser(tokens);

				parser.topLevel();
				long stop = System.nanoTime();

				int errors = parser.getNumberOfSyntaxErrors();
				if (errors >= 1) {
					System.out.println(String.format("%s SYNTAX ERROR",f));
					failed = true;
					return;
				}
				lexerTime = stop - start;
				if (lexerTime < fastest || fastest == 0) {
					fastest = lexerTime;
				}
			}
			double result = fastest / 1000000.0;
			System.out.println(String.format("%s %s", f, result));
		} catch (Exception e) {
			// System.err.println("parser exception: "+e);
			// e.printStackTrace(); // so we can get stack trace
		}
	}

}
