import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;
import java.io.*;

class Main {
  public static boolean failed = false;
    public static void main(String args[]) throws Exception {
      for (String path : args) {
        parse(path);
      }
    }

    public static void parse(String path) throws Exception{
      long fastest = 0;
    	for(int i=0;i<10;i++) {
      	ANTLRFileStream input = new ANTLRFileStream(path);
      	long t1 = System.nanoTime();
        XMLLexer lexer = new XMLLexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        XMLParser parser = new XMLParser(tokens);
        ParseTree tree = parser.document(); // begin parsing at query rule
      	long t2 = System.nanoTime(); // ----->
      	if( (t2-t1) < fastest || fastest == 0 ) {
      		fastest = (t2-t1);
      	}
        int errors = parser.getNumberOfSyntaxErrors();
        if( errors >= 1 ) {
          System.out.println(String.format("%s FAIL",path));
          failed = true;
          break;
        }
    	}
      double msTime = (double) fastest / 1000000.0;
      System.out.println(String.format("%s %f",path,msTime));

    }
}
