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
	// static JavaScriptLexer lexer;
	static CelloLexer lexer;

	public static void main(String[] args) {
		try {
			// long start = System.currentTimeMillis();
			// long start = System.nanoTime();
			if (args.length > 0) {
				// for each directory/file specified on the command line
				for (int i = 0; i < args.length; i++) {
					// doFile(new File(args[i])); // parse it
					parseFile(args[i]);
				}
			}
			else {
				System.err.println("Usage: java Main <directory or file name>");
			}
			// long stop = System.currentTimeMillis();
			// long stop = System.nanoTime();
			// System.out.println("Lexed in " +lexerTime+ "ms.");
			// System.out.println("Overall parse in " + (stop - start) + "ms.");
			// System.out.println("Overall parse in " + (stop - start) + "ns.");

			// System.out.println((stop - start));

			FileWriter fw = new FileWriter("parseTime.txt", false);
			PrintWriter pw = new PrintWriter(new BufferedWriter(fw));
			if (!failed) {
				pw.println(lexerTime);
			} else {
				pw.println("ERROR");
			}
			pw.close();

			// System.out.println("finished parsing OK");
			if (profile) {
				System.out.println("num decisions " + profiler.numDecisions);
			}
		} catch (Exception e) {

			// System.err.println("exception: "+e);
			// e.printStackTrace(System.err); // so we can get stack trace
		}
	}


	// This method decides what action to take based on the type of
	// file we are looking at
	public static void doFile(File f)
			throws Exception {
		// If this is a directory, walk each file/dir in that directory
		if (f.isDirectory()) {
			String files[] = f.list();
			for (int i = 0; i < files.length; i++)
				doFile(new File(f, files[i]));
		}
		// otherwise, if this is a java file, parse it!
		else if (((f.getName().length() > 5) &&
				f.getName().substring(f.getName().length() - 5).equals(".java"))
				|| f.getName().equals("input"))
		{
			parseFile(f.getAbsolutePath());
		}
	}

	static class CountDecisions extends BlankDebugEventListener {
		public int numDecisions = 0;

		public void enterDecision(int decisionNumber) {
			numDecisions++;
		}
	}

	static CountDecisions profiler = new CountDecisions();

	// Here's where we do the real work...
	public static void parseFile(String f) throws Exception {
		long fastest = 0;
		try {
			for (int i = 0; i < 10; i++) {
				// Create a scanner that reads from the input stream passed to
				// us
				if (lexer == null) {
					// lexer = new JavaScriptLexer();
					lexer = new CelloLexer();
				}

				lexer.setCharStream(new ANTLRFileStream(f));
				CommonTokenStream tokens = new CommonTokenStream(lexer);
				long start = System.nanoTime();
				tokens.fill(); // force load
				// long stop = System.currentTimeMillis();
				// lexerTime += stop-start;

				// System.out.println(tokens);

				// Create a parser that reads from the scanner

				// JavaParser parser = null;

				CelloParser parser = new CelloParser(tokens);
				// CommonTree tree = (CommonTree)parser.compilationUnit();
				// int errors = parser.getNumberOfSyntaxErrors();
				// if( errors >= 1 ) {
				// failed = true;
				// return;
				// }
				// System.out.println("error = " + errors);
				// System.out.println(tree.toStringTree());
				parser.topLevel();
				long stop = System.nanoTime();

				int errors = parser.getNumberOfSyntaxErrors();
				if (errors >= 1) {
					System.out.println("SYNTAX ERROR");
					failed = true;
					return;
				}

				lexerTime = stop - start;
				if (lexerTime < fastest || fastest == 0) {
					fastest = lexerTime;
				}
			}
			lexerTime = fastest;
			// System.out.println(String.format("%s,%s",f,fastest));
			// CommonTree root = parser.compilationUnit().tree;
			// System.out.println(root.toStringTree());

			// CommonTree root = parser.program().tree;
			// System.out.println(parser.program().tree.toString());

			// parser = new JavaParser(tokens);

			// start parsing at the compilationUnit rule
			// parser.compilationUnit();
			// System.err.println("finished "+f);
		} catch (Exception e) {
			// System.err.println("parser exception: "+e);
			// e.printStackTrace(); // so we can get stack trace
		}
	}

}
