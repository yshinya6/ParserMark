import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

import org.antlr.v4.runtime.ANTLRFileStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;

class Main {
	public static boolean failed = false;

	public static void main(String args[]) throws Exception {

		if (args.length != 1) {
			System.err.println("This is a pmark benchmark program for ANTLR4");
			System.err.println("Usage: <Input>");
			System.exit(1);
		}
		parse(args[0]);

		// System.out.println(tree.toStringTree(parser)); // print LISP-style
		// tree
		// System.out.println(parser.compilationUnit().toString()); // begin
		// parsing at query rule

	}

	public static void parse(String filePath) throws IOException {
		long fastest = 0;
		for (int i = 0; i < 10; i++) {

			ANTLRFileStream input = new ANTLRFileStream(filePath);

			long t1 = System.nanoTime();
			CelloLexer lexer = new CelloLexer(input);
			CommonTokenStream tokens = new CommonTokenStream(lexer);
			CelloParser parser = new CelloParser(tokens);
			ParseTree tree = parser.topLevel();
//			System.out.println(tree.toStringTree(parser)); // print
			long t2 = System.nanoTime();

			if ((t2 - t1) < fastest || fastest == 0) {
				fastest = (t2 - t1);
			}
			int errors = parser.getNumberOfSyntaxErrors();
			if (errors >= 1) {
				System.out.println(String.format("%s ERROR",filePath));
				failed = true;
				break;
			}
			// System.out.println((t2-t1));
		}
		System.out.println(String.format("%s %s", filePath, fastest));

		FileWriter fw = new FileWriter("parseTime.txt", false);
		PrintWriter pw = new PrintWriter(new BufferedWriter(fw));
		if (!failed) {
			pw.println(fastest);
		} else {
			pw.println("ERROR");
		}
		pw.close();
	}

}
