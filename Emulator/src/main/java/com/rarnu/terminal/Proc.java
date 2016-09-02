package com.rarnu.terminal;

import java.io.FileDescriptor;

public class Proc {
    static {
        System.loadLibrary("term");
    }

    public static native FileDescriptor createSubprocess(String cmd, String[] args, String[] envVars, int[] processId);

    public static native void setPtyWindowSize(FileDescriptor fd, int row, int col, int xpixel, int ypixel);

    public static native void setPtyUTF8Mode(FileDescriptor fd, boolean utf8Mode);

    public static native int waitFor(int processId);

    public static native void close(FileDescriptor fd);

    public static native void hangupProcessGroup(int processId);
}
