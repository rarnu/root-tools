package com.rarnu.command.emu;

import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import android.os.Handler;
import android.os.Message;

import com.rarnu.command.emu.event.IEmuCallback;
import com.rarnu.command.jni.Exec;

public class EmulatorTool {

	private final static String DEFAULT_SHELL = "/system/bin/sh -";

	private final static String DEFAULT_INITIAL_COMMAND = "export PATH=/data/local/bin:$PATH";

	private TranscriptScreen mTranscriptScreen;

	private TerminalEmulator mEmulator;

	private FileDescriptor mTermFd;

	private FileInputStream mTermIn;

	private FileOutputStream mTermOut;

	private ByteQueue mByteQueue;

	private byte[] mReceiveBuffer;

	private static final int UPDATE = 1;

	private Thread mPollingThread;
	
	private IEmuCallback callback;

	private final Handler mHandler = new Handler() {

		@Override
		public void handleMessage(Message msg) {
			if (msg.what == UPDATE) {
				update();
			}
		}
	};

	public EmulatorTool(IEmuCallback callback) {
		this.callback = callback;
	}
	
	public void close() {
		if (mPollingThread.isAlive()) {
			mPollingThread.interrupt();
		}
		if (callback != null) {
			callback.closingEmu();
		}
		if (mTermFd != null) {
			Exec.close(mTermFd);
			mTermFd = null;
		}
	}

	public void resetTerminal() {
		mEmulator.reset();
	}

	public void listen() {
		startListening();
	}

	public void initialize(FileDescriptor termFd, FileOutputStream termOut) {
		mTranscriptScreen = new TranscriptScreen(1, 10000, 0, 0, 7);
		mEmulator = new TerminalEmulator(mTranscriptScreen, 1, 0, mTermOut);
		mTermOut = termOut;
		mTermFd = termFd;
		mTermIn = new FileInputStream(mTermFd);
		mReceiveBuffer = new byte[4 * 1024];
		mByteQueue = new ByteQueue(4 * 1024);
	}

	public void append(byte[] buffer, int base, int length) {
		mEmulator.append(buffer, base, length);
	}

	public void write(String data) {
		try {
			mTermOut.write(data.getBytes());
			mTermOut.flush();
		} catch (IOException e) {
		}
	}

	private String msgStr = "";

	private void update() {
		int bytesAvailable = mByteQueue.getBytesAvailable();
		int bytesToRead = Math.min(bytesAvailable, mReceiveBuffer.length);
		try {
			int bytesRead = mByteQueue.read(mReceiveBuffer, 0, bytesToRead);
			append(mReceiveBuffer, 0, bytesRead);
		} catch (InterruptedException e) {
		}
	}

	private void startListening() {
		int[] processId = new int[1];

		createSubprocess(processId);
		final int procId = processId[0];

		final Handler handler = new Handler() {
			@Override
			public void handleMessage(Message msg) {
			}
		};

		Runnable watchForDeath = new Runnable() {
			public void run() {
				int result = Exec.waitFor(procId);
				handler.sendEmptyMessage(result);
			}

		};
		Thread watcher = new Thread(watchForDeath);
		watcher.start();
		mTermOut = new FileOutputStream(mTermFd);
		initialize(mTermFd, mTermOut);
		sendInitialCommand();
		
		mPollingThread = new Thread(new Runnable() {

			public void run() {
				try {
					while (true) {
						int read = mTermIn.read(mBuffer);
						mByteQueue.write(mBuffer, 0, read);
						msgStr = new String(mBuffer).trim();
						if (callback != null) {
							callback.receiveEmuMessage(msgStr);
						}
						mHandler.sendMessage(mHandler.obtainMessage(UPDATE));
					}
				} catch (IOException e) {
				} catch (InterruptedException e) {
				}
			}

			private byte[] mBuffer = new byte[4096];
		});
		mPollingThread.setName("Input reader");
		mPollingThread.start();
	}

	private void sendInitialCommand() {
		String initialCommand = DEFAULT_INITIAL_COMMAND;

		if (initialCommand.length() > 0) {
			write(initialCommand + '\r');
		}
		
		if (callback != null) {
			callback.openingEmu();
		}
	}

	private void createSubprocess(int[] processId) {
		String shell = DEFAULT_SHELL;

		ArrayList<String> args = parse(shell);
		String arg0 = args.get(0);
		String arg1 = null;
		String arg2 = null;
		if (args.size() >= 2) {
			arg1 = args.get(1);
		}
		if (args.size() >= 3) {
			arg2 = args.get(2);
		}
		mTermFd = Exec.createSubprocess(arg0, arg1, arg2, processId);
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

}