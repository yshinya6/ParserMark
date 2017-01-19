package bench;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.lang.reflect.InvocationTargetException;

import xtc.parser.ParseError;
import xtc.parser.Result;

public class Main {

	public static long calcExecTime(long[] timeList) {
		long fastestTime = timeList[0];
		for (long l : timeList) {
			if (fastestTime > l) {
				fastestTime = l;
			}
		}
		return fastestTime;
	}

	public static void main(String[] args) {
		long startTime;
		long endTime;
		long aveTime = 0;
		long[] elapsedTimeList = new long[10];


		if (args.length < 2) {
			System.out.println("Usage: <ParserClassName> <file>");
			// showExistClasses();
			System.exit(0);
		}

		Class<?> parserClass = loadClass(args[0]);
		if (args[0].equals("java")) {

		}

		for (int argsIndex = 1; argsIndex < args.length; argsIndex++) {
			String fileName = args[argsIndex].substring(args[argsIndex].lastIndexOf("/") + 1);
			long fileSize = new File(args[argsIndex]).length();


			for (int itr = 0; itr < 10; itr++) {
				try {
					FileInputStream fis = new FileInputStream(args[argsIndex]);

					BufferedInputStream bis = new BufferedInputStream(fis);
					InputStreamReader isr = new InputStreamReader(bis);
					PEGParser parser = (PEGParser) parserClass.getConstructor(Reader.class, String.class)
							.newInstance(isr, args[argsIndex]);
					startTime = System.nanoTime();
//					Result result = (Result) method.invoke(parser, 0);
					Result result = parser.pFile(0);
					endTime = System.nanoTime();
					elapsedTimeList[itr] = endTime - startTime;
					// System.out.println(elapsedTimeList[itr] + "[ms]");
					if (result instanceof ParseError) {
						break;
					}
				} catch (IOException e) {
					e.printStackTrace();
				} catch (IllegalAccessException e) {
					e.printStackTrace();
				} catch (IllegalArgumentException e) {
					e.printStackTrace();
				} catch (InvocationTargetException e) {
					e.printStackTrace();
				} catch (NoSuchMethodException e) {
					e.printStackTrace();
				} catch (SecurityException e) {
					e.printStackTrace();
				} catch (InstantiationException e) {
					e.printStackTrace();
				} catch (StackOverflowError e) {
					break;
				}
			}
			for (int i = 1; i < elapsedTimeList.length; i++) {
				aveTime += elapsedTimeList[i];
			}
			long fastest = calcExecTime(elapsedTimeList);
			if (fastest == 0) {
				String msg = String.format("%s,%s,%s", fileName, fileSize, "failed");
				System.out.println(msg);
			} else {
				String msg = String.format("%s,%s,%s", fileName, fileSize, ((double) (aveTime / 9.0))  /1000000.0);
				System.out.println(msg);
			}
		}
	}

	private static final Class<?> loadClass(String className) {
		try {
			return Class.forName("resource." + className);
		} catch (ClassNotFoundException e) {
			System.out.println("no such a parser class: " + className);
			System.exit(1);
		}
		return null;
	}

	// private static final void showExistClasses(){
	// ClassLoader loader = Thread.currentThread().getContextClassLoader();
	// URL url = loader.getResource("resource");
	//
	// }

	public interface PEGParser {
		public Result pFile(int yyStart) throws IOException;
	}

}
