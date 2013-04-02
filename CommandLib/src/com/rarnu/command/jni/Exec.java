package com.rarnu.command.jni;

import java.io.FileDescriptor;

public class Exec {
	static {
		System.loadLibrary("term");
	}

	public static native FileDescriptor createSubprocess(String cmd,
			String arg0, String arg1, int[] processId);

	public static native void setPtyWindowSize(FileDescriptor fd, int row,
			int col, int xpixel, int ypixel);

	public static native int waitFor(int processId);

	public static native void close(FileDescriptor fd);
}
