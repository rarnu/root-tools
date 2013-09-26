package com.rarnu.terminal.session;

import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import com.rarnu.terminal.TerminalEmulator;
import com.rarnu.terminal.callback.FinishCallback;
import com.rarnu.terminal.callback.ReturnDataCallback;
import com.rarnu.terminal.callback.UpdateCallback;
import com.rarnu.terminal.renderer.BaseTextRenderer;
import com.rarnu.terminal.screen.TranscriptScreen;
import com.rarnu.terminal.utils.ByteQueue;
import com.rarnu.terminal.utils.ColorScheme;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CodingErrorAction;

public class TermSession {
    private static final int TRANSCRIPT_ROWS = 10000;
    private static final int NEW_INPUT = 1;
    private static final int NEW_OUTPUT = 2;
    private static final int FINISH = 3;
    private ColorScheme mColorScheme = BaseTextRenderer.defaultColorScheme;
    private UpdateCallback mNotify;
    private ReturnDataCallback mDataCallback;
    private OutputStream mTermOut;
    private InputStream mTermIn;
    private String mTitle;
    private TranscriptScreen mTranscriptScreen;
    private TerminalEmulator mEmulator;
    private boolean mDefaultUTF8Mode;
    private Thread mReaderThread;
    private ByteQueue mByteQueue;
    private byte[] mReceiveBuffer;
    private Thread mWriterThread;
    private ByteQueue mWriteQueue;
    private Handler mWriterHandler;
    private CharBuffer mWriteCharBuffer;
    private ByteBuffer mWriteByteBuffer;
    private CharsetEncoder mUTF8Encoder;
    private FinishCallback mFinishCallback;
    private boolean mIsRunning = false;
    private Handler mMsgHandler = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (!mIsRunning) {
                return;
            }
            if (msg.what == NEW_INPUT) {
                readFromProcess();
            }
        }
    };
    private UpdateCallback mTitleChangedListener;

    public TermSession() {
        mWriteCharBuffer = CharBuffer.allocate(2);
        mWriteByteBuffer = ByteBuffer.allocate(4);
        mUTF8Encoder = Charset.forName("UTF-8").newEncoder();
        mUTF8Encoder.onMalformedInput(CodingErrorAction.REPLACE);
        mUTF8Encoder.onUnmappableCharacter(CodingErrorAction.REPLACE);

        mReceiveBuffer = new byte[4 * 1024];
        mByteQueue = new ByteQueue(4 * 1024);
        mReaderThread = new Thread() {
            private byte[] mBuffer = new byte[4096];

            @Override
            public void run() {
                try {
                    while (true) {
                        int read = mTermIn.read(mBuffer);
                        if (read == -1) {
                            return;
                        }
                        int offset = 0;
                        while (read > 0) {
                            int written = mByteQueue.write(mBuffer, offset, read);
                            offset += written;
                            read -= written;
                            mMsgHandler.sendMessage(mMsgHandler.obtainMessage(NEW_INPUT));
                        }
                    }
                } catch (IOException e) {
                } catch (InterruptedException e) {
                }
            }
        };
        mReaderThread.setName("TermSession input reader");

        mWriteQueue = new ByteQueue(4096);
        mWriterThread = new

                Thread() {
                    private byte[] mBuffer = new byte[4096];

                    @Override
                    public void run() {
                        Looper.prepare();

                        mWriterHandler = new Handler() {
                            @Override
                            public void handleMessage(Message msg) {
                                if (msg.what == NEW_OUTPUT) {
                                    writeToOutput();
                                } else if (msg.what == FINISH) {
                                    Looper.myLooper().quit();
                                }
                            }
                        };
                        writeToOutput();
                        Looper.loop();
                    }

                    private void writeToOutput() {
                        ByteQueue writeQueue = mWriteQueue;
                        byte[] buffer = mBuffer;
                        OutputStream termOut = mTermOut;

                        int bytesAvailable = writeQueue.getBytesAvailable();
                        int bytesToWrite = Math.min(bytesAvailable, buffer.length);

                        if (bytesToWrite == 0) {
                            return;
                        }

                        try {
                            writeQueue.read(buffer, 0, bytesToWrite);
                            termOut.write(buffer, 0, bytesToWrite);
                            termOut.flush();
                        } catch (Exception e) {

                        }
                    }
                };
        mWriterThread.setName("TermSession output writer");
    }

    public void initializeEmulator(int columns, int rows) {
        mTranscriptScreen = new TranscriptScreen(columns, TRANSCRIPT_ROWS, rows, mColorScheme);
        mEmulator = new TerminalEmulator(this, mTranscriptScreen, columns, rows, mColorScheme);
        mEmulator.setDefaultUTF8Mode(mDefaultUTF8Mode);

        mIsRunning = true;
        mReaderThread.start();
        mWriterThread.start();
    }

    public void setReturnDataCallback(ReturnDataCallback callback) {
        mDataCallback = callback;
    }

    public void write(byte[] data, int offset, int count) {
        try {
            while (count > 0) {
                int written = mWriteQueue.write(data, offset, count);
                offset += written;
                count -= written;
                notifyNewOutput();
            }
        } catch (InterruptedException e) {
        }
    }

    public void write(String data) {
        try {
            byte[] bytes = data.getBytes("UTF-8");
            write(bytes, 0, bytes.length);
        } catch (UnsupportedEncodingException e) {
        }
    }

    public void write(int codePoint) {
        CharBuffer charBuf = mWriteCharBuffer;
        ByteBuffer byteBuf = mWriteByteBuffer;
        CharsetEncoder encoder = mUTF8Encoder;

        charBuf.clear();
        byteBuf.clear();
        Character.toChars(codePoint, charBuf.array(), 0);
        encoder.reset();
        encoder.encode(charBuf, byteBuf, true);
        encoder.flush(byteBuf);
        write(byteBuf.array(), 0, byteBuf.position() - 1);
    }

    private void notifyNewOutput() {
        Handler writerHandler = mWriterHandler;
        if (writerHandler == null) {
            return;
        }
        writerHandler.sendEmptyMessage(NEW_OUTPUT);
    }

    public OutputStream getTermOut() {
        return mTermOut;
    }

    public void setTermOut(OutputStream termOut) {
        mTermOut = termOut;
    }

    public InputStream getTermIn() {
        return mTermIn;
    }

    public void setTermIn(InputStream termIn) {
        mTermIn = termIn;
    }

    public boolean isRunning() {
        return mIsRunning;
    }

    public TranscriptScreen getTranscriptScreen() {
        return mTranscriptScreen;
    }

    public TerminalEmulator getEmulator() {
        return mEmulator;
    }

    public void setUpdateCallback(UpdateCallback notify) {
        mNotify = notify;
    }

    protected void notifyUpdate() {
        if (mNotify != null) {
            mNotify.onUpdate();
        }
    }

    public String getTitle() {
        return mTitle;
    }

    public void setTitle(String title) {
        mTitle = title;
        notifyTitleChanged();
    }

    public void setTitleChangedListener(UpdateCallback listener) {
        mTitleChangedListener = listener;
    }

    protected void notifyTitleChanged() {
        UpdateCallback listener = mTitleChangedListener;
        if (listener != null) {
            listener.onUpdate();
        }
    }

    public void updateSize(int columns, int rows) {
        if (mEmulator == null) {
            initializeEmulator(columns, rows);
        } else {
            mEmulator.updateSize(columns, rows);
        }
    }

    public String getTranscriptText() {
        return mTranscriptScreen.getTranscriptText();
    }

    private void readFromProcess() {
        int bytesAvailable = mByteQueue.getBytesAvailable();
        int bytesToRead = Math.min(bytesAvailable, mReceiveBuffer.length);
        int bytesRead = 0;
        try {
            bytesRead = mByteQueue.read(mReceiveBuffer, 0, bytesToRead);
        } catch (InterruptedException e) {
            return;
        }

        processInput(mReceiveBuffer, 0, bytesRead);
        notifyUpdate();
    }

    protected void processInput(byte[] data, int offset, int count) {
        if (mDataCallback != null) {
            mDataCallback.onReceiveData(processInputString(data, offset, count));
        }
        mEmulator.append(data, offset, count);
    }

    private String processInputString(byte[] data, int offset, int count) {
        String str = "";
        for (int i = 0; i < count; i++) {
            byte b = data[offset + i];
            try {
                str += (char) b;
            } catch (Exception e) {

            }
        }
        return str;
    }

    protected final void appendToEmulator(byte[] data, int offset, int count) {
        mEmulator.append(data, offset, count);
    }

    public void setColorScheme(ColorScheme scheme) {
        if (scheme == null) {
            scheme = BaseTextRenderer.defaultColorScheme;
        }
        mColorScheme = scheme;
        if (mEmulator == null) {
            return;
        }
        mEmulator.setColorScheme(scheme);
        mTranscriptScreen.setColorScheme(scheme);
    }

    public void setDefaultUTF8Mode(boolean utf8ByDefault) {
        mDefaultUTF8Mode = utf8ByDefault;
        if (mEmulator == null) {
            return;
        }
        mEmulator.setDefaultUTF8Mode(utf8ByDefault);
    }

    public boolean getUTF8Mode() {
        if (mEmulator == null) {
            return mDefaultUTF8Mode;
        } else {
            return mEmulator.getUTF8Mode();
        }
    }

    public void setUTF8ModeUpdateCallback(UpdateCallback utf8ModeNotify) {
        if (mEmulator != null) {
            mEmulator.setUTF8ModeUpdateCallback(utf8ModeNotify);
        }
    }

    public void reset() {
        mEmulator.reset();
        notifyUpdate();
    }

    public void setFinishCallback(FinishCallback callback) {
        mFinishCallback = callback;
    }

    public void finish() {
        mIsRunning = false;
        if (mTranscriptScreen != null) {
            mTranscriptScreen.finish();
        }

        if (mWriterHandler != null) {
            mWriterHandler.sendEmptyMessage(FINISH);
        }
        try {
            mTermIn.close();
            mTermOut.close();
        } catch (Exception e) {

        }

        if (mFinishCallback != null) {
            mFinishCallback.onSessionFinish(this);
        }
    }
}
