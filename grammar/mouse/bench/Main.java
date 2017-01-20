package bench;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import mouse.runtime.Source;
import mouse.runtime.SourceFile;

public class Main {
	public static void main(String[] args) {
		long startTime;
		long endTime;
		long[] elapsedTimeList = new long[10];
		long fileSize;
		if (args.length != 2) {
			System.out.println("Usage: <ParserClassName> <InputFile>");
			System.out.println("(example) XMark xmark5m.xml");
			System.exit(1);
		}
		Class<?> parserClass = loadClass(args[0]);
		SourceFile sourceFile = new SourceFile(args[1]);
		fileSize = sourceFile.file().length();


		//System.out.println("[" + args[1] + "]");

		for (int i = 0; i < 10; i++) {
			try {
				Object parser = parserClass.getConstructor().newInstance();
				Method method = parser.getClass().getMethod("parse", Source.class);
				startTime = System.nanoTime();
				boolean result = (boolean) method.invoke(parser, sourceFile);
				endTime = System.nanoTime();
				elapsedTimeList[i] = endTime - startTime;
				//System.out.println(elapsedTimeList[i] + "[ms]");
				if (!result) {
					System.out.println("Syntax Error");
					System.exit(1);
				}
			} catch (InstantiationException | IllegalAccessException | IllegalArgumentException
					| InvocationTargetException | NoSuchMethodException | SecurityException e) {
				e.printStackTrace();
			}
		}
		double fastestTime = (double) calcExecTime(elapsedTimeList) / 1000000.0;
		System.out.println(args[1] + "  " + fastestTime + " [ms]");
	}

	private static long calcExecTime(long[] timeList) {
		long fastestTime = timeList[0];
		for (long l : timeList) {
			if (fastestTime > l) {
				fastestTime = l;
			}
		}
		return fastestTime;
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
}
