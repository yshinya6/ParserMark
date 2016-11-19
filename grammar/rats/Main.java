
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


		if (args.length < 1) {
			System.out.println("Usage: <file>+");
			// showExistClasses();
			System.exit(0);
		}


		for (int argsIndex = 0; argsIndex < args.length; argsIndex++) {
			String fileName = args[argsIndex].substring(args[argsIndex].lastIndexOf("/") + 1);
			long fileSize = new File(args[argsIndex]).length();


			for (int itr = 0; itr < 10; itr++) {
				try {
					FileInputStream fis = new FileInputStream(args[argsIndex]);
					BufferedInputStream bis = new BufferedInputStream(fis);
					InputStreamReader isr = new InputStreamReader(bis);
//					cello parser = new cello(isr, args[argsIndex]);
					cello parser = new cello(isr, args[argsIndex]);
					startTime = System.nanoTime();
//					Result result = (Result) method.invoke(parser, 0);
					Result result = parser.pFile(0);
					endTime = System.nanoTime();
					elapsedTimeList[itr] = endTime - startTime;

					// System.out.println(elapsedTimeList[itr] + " [ms]");
					if (result instanceof ParseError) {
						System.out.println("pos:"+ ((ParseError) result).index + " " +((ParseError) result).msg);
						break;
					}
				} catch (IOException e) {
					e.printStackTrace();
				} catch (StackOverflowError e) {
					e.printStackTrace();
					break;
				}
			}
			for (int i = 1; i < elapsedTimeList.length; i++) {
				aveTime += elapsedTimeList[i];
			}
			long fastest = calcExecTime(elapsedTimeList);
			if (fastest == 0) {
				String msg = String.format("%s %s %s", fileName, fileSize, "failed");
				System.out.println(msg);
			} else {
				String msg = String.format("%s %s %s", fileName, fileSize, fastest);
				System.out.println(msg);
			}
		}
	}

	// private static final void showExistClasses(){
	// ClassLoader loader = Thread.currentThread().getContextClassLoader();
	// URL url = loader.getResource("resource");
	//
	// }

}
