package com.rarnu.terminal.session;

import android.os.Handler;
import android.os.Message;
import com.rarnu.terminal.Proc;
import com.rarnu.terminal.callback.UpdateCallback;

import java.io.*;
import java.util.ArrayList;

public class ShellTermSession extends TermSession {

    public static final int PROCESS_EXIT_FINISHES_SESSION = 0;
    public static final int PROCESS_EXIT_DISPLAYS_MESSAGE = 1;
    private static final boolean VTTEST_MODE = false;
    private static final int PROCESS_EXITED = 1;
    private int mProcId;
    private FileDescriptor mTermFd;
    private Thread mWatcherThread;
    private String mHandle;
    private String mInitialCommand;
    private String mProcessExitMessage;
    private Handler mMsgHandler = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (!isRunning()) {
                return;
            }
            if (msg.what == PROCESS_EXITED) {
                onProcessExit((Integer) msg.obj);
            }
        }
    };
    private UpdateCallback mUTF8ModeNotify = new UpdateCallback() {
        public void onUpdate() {
            Proc.setPtyUTF8Mode(mTermFd, getUTF8Mode());
        }
    };

    public ShellTermSession(String initialCommand) {
        super();
        initializeSession();
        mInitialCommand = initialCommand;
        mWatcherThread = new Thread() {
            @Override
            public void run() {
                int result = Proc.waitFor(mProcId);
                mMsgHandler.sendMessage(mMsgHandler.obtainMessage(PROCESS_EXITED, result));
            }
        };
        mWatcherThread.setName("Process watcher");
    }

    private void initializeSession() {

        int[] processId = new int[1];
        String[] env = new String[2];
        env[0] = "TERM=screen";
        env[1] = "PATH=" + System.getenv("PATH");

        createSubprocess(processId, "/system/bin/sh -", env);
        mProcId = processId[0];

        setTermOut(new FileOutputStream(mTermFd));
        setTermIn(new FileInputStream(mTermFd));
    }

    @Override
    public void initializeEmulator(int columns, int rows) {
        if (VTTEST_MODE) {
            columns = 80;
            rows = 24;
        }
        super.initializeEmulator(columns, rows);

        Proc.setPtyUTF8Mode(mTermFd, getUTF8Mode());
        setUTF8ModeUpdateCallback(mUTF8ModeNotify);

        mWatcherThread.start();
        sendInitialCommand(mInitialCommand);
    }

    private void sendInitialCommand(String initialCommand) {
        if (initialCommand.length() > 0) {
            write(initialCommand + '\r');
        }
    }

    private void createSubprocess(int[] processId, String shell, String[] env) {
        ArrayList<String> argList = parse(shell);
        String arg0;
        String[] args;

        try {
            arg0 = argList.get(0);
            File file = new File(arg0);
            if (!file.exists()) {

                throw new FileNotFoundException(arg0);
            } else if (!file.canExecute()) {

                throw new FileNotFoundException(arg0);
            }
            args = argList.toArray(new String[1]);
        } catch (Exception e) {
            argList = parse("/system/bin/sh -");
            arg0 = argList.get(0);
            args = argList.toArray(new String[1]);
        }

        mTermFd = Proc.createSubprocess(arg0, args, env, processId);
    }

    private ArrayList<String> parse(String cmd) {
        final int PLAIN = 0;
        final int WHITESPACE = 1;
        final int INQUOTE = 2;
        int state = WHITESPACE;
        ArrayList<String> result = new ArrayList<String>();
        int cmdLen = cmd.length();
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < cmdLen; i++) {
            char c = cmd.charAt(i);
            if (state == PLAIN) {
                if (Character.isWhitespace(c)) {
                    result.add(builder.toString());
                    builder.delete(0, builder.length());
                    state = WHITESPACE;
                } else if (c == '"') {
                    state = INQUOTE;
                } else {
                    builder.append(c);
                }
            } else if (state == WHITESPACE) {
                if (Character.isWhitespace(c)) {

                } else if (c == '"') {
                    state = INQUOTE;
                } else {
                    state = PLAIN;
                    builder.append(c);
                }
            } else if (state == INQUOTE) {
                if (c == '\\') {
                    if (i + 1 < cmdLen) {
                        i += 1;
                        builder.append(cmd.charAt(i));
                    }
                } else if (c == '"') {
                    state = PLAIN;
                } else {
                    builder.append(c);
                }
            }
        }
        if (builder.length() > 0) {
            result.add(builder.toString());
        }
        return result;
    }

    @Override
    public void updateSize(int columns, int rows) {
        if (VTTEST_MODE) {
            columns = 80;
            rows = 24;
        }
        Proc.setPtyWindowSize(mTermFd, rows, columns, 0, 0);
        super.updateSize(columns, rows);
    }

    public void setProcessExitMessage(String message) {
        mProcessExitMessage = message;
    }

    private void onProcessExit(int result) {
        if (mProcessExitMessage != null) {
            try {
                byte[] msg = ("\r\n[" + mProcessExitMessage + "]")
                        .getBytes("UTF-8");
                appendToEmulator(msg, 0, msg.length);
                notifyUpdate();
            } catch (UnsupportedEncodingException e) {

            }
        }
    }

    @Override
    public void finish() {
        Proc.hangupProcessGroup(mProcId);
        Proc.close(mTermFd);
        super.finish();
    }

    public String getTitle(String defaultTitle) {
        String title = super.getTitle();
        if (title != null && title.length() > 0) {
            return title;
        } else {
            return defaultTitle;
        }
    }

    public String getHandle() {
        return mHandle;
    }

    public void setHandle(String handle) {
        if (mHandle != null) {
            throw new IllegalStateException("Cannot change handle once set");
        }
        mHandle = handle;
    }
}
