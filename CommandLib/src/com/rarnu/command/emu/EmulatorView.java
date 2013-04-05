package com.rarnu.command.emu;

import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Rect;
import android.os.Handler;
import android.os.Message;
import android.util.AttributeSet;
import android.view.KeyEvent;
import android.view.MotionEvent;
import android.view.View;
import android.view.inputmethod.BaseInputConnection;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputConnection;

import com.rarnu.command.emu.event.ITerminalCallback;
import com.rarnu.command.emu.renderer.PaintRenderer;
import com.rarnu.command.emu.renderer.TextRenderer;
import com.rarnu.command.emu.tool.ByteQueue;
import com.rarnu.command.emu.tool.TermKeyListener;
import com.rarnu.command.emu.tool.TranscriptScreen;
import com.rarnu.command.jni.Exec;

public class EmulatorView extends View {

	private final static String DEFAULT_SHELL = "/system/bin/sh -";
	private final static String DEFAULT_INITIAL_COMMAND = "export PATH=/data/local/bin:$PATH";
	private boolean mKnownSize;
	private int mVisibleWidth;
	private int mVisibleHeight;
	private Rect mVisibleRect = new Rect();
	private int mTitleHeight;
	private TranscriptScreen mTranscriptScreen;
	private static final int TRANSCRIPT_ROWS = 10000;
	private int mCharacterWidth;
	private int mCharacterHeight;
	private TextRenderer mTextRenderer;
	private int mTextSize;
	private int mCursorBlink;
	private int mForeground;
	private int mBackground;
	private Paint mCursorPaint;
	private Paint mBackgroundPaint;
	private boolean mUseCookedIme;
	private TerminalEmulator mEmulator;
	private int mRows;
	private int mColumns;
	private int mVisibleColumns;
	private int mTopRow;
	private int mLeftColumn;
	private FileDescriptor mTermFd;
	private FileInputStream mTermIn;
	private FileOutputStream mTermOut;
	private ByteQueue mByteQueue;
	private byte[] mReceiveBuffer;
	private static final int UPDATE = 1;
	private static final int SCREEN_CHECK_PERIOD = 1000;
	private static final int CURSOR_BLINK_PERIOD = 1000;
	private boolean mCursorVisible = true;
	private Thread mPollingThread;
	private float mScrollRemainder;
	private TermKeyListener mKeyListener;
	private ITerminalCallback mTerminalCallback;

	private Runnable mCheckSize = new Runnable() {
		public void run() {
			updateSize(false);
			mHandler.postDelayed(this, SCREEN_CHECK_PERIOD);
		}
	};

	private Runnable mBlinkCursor = new Runnable() {
		public void run() {
			if (mCursorBlink != 0) {
				mCursorVisible = !mCursorVisible;
				mHandler.postDelayed(this, CURSOR_BLINK_PERIOD);
			} else {
				mCursorVisible = true;
			}
			invalidate();
		}
	};

	private final Handler mHandler = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == UPDATE) {
				update();
			}
		}
	};

	public EmulatorView(Context context) {
		super(context);
		commonConstructor();
	}

	public EmulatorView(Context context, AttributeSet attrs) {
		this(context, attrs, 0);
	}

	public EmulatorView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		commonConstructor();
	}
	
	public void close() {
		if (mPollingThread.isAlive()) {
			mPollingThread.interrupt();
		}
		if (mTermFd != null) {
			Exec.close(mTermFd);
			mTermFd = null;
		}
	}

	public void onResume() {
		updateSize(false);
		mHandler.postDelayed(mCheckSize, SCREEN_CHECK_PERIOD);
		if (mCursorBlink != 0) {
			mHandler.postDelayed(mBlinkCursor, CURSOR_BLINK_PERIOD);
		}
	}

	public void onPause() {
		mHandler.removeCallbacks(mCheckSize);
		if (mCursorBlink != 0) {
			mHandler.removeCallbacks(mBlinkCursor);
		}
	}

	public void register(ITerminalCallback terminalCallback,
			TermKeyListener listener) {
		mTerminalCallback = terminalCallback;
		mKeyListener = listener;
	}

	public void setColors(int foreground, int background) {
		mForeground = foreground;
		mBackground = background;
		updateText();
	}

	public String getTranscriptText() {
		return mEmulator.getTranscriptText();
	}

	public void resetTerminal() {
		mEmulator.reset();
		invalidate();
	}

	public void startListening(String initCommand) {
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
		writeCommand(DEFAULT_INITIAL_COMMAND + "\r");
		writeCommand(initCommand);
	}

	public void writeCommand(String data) {
		try {
			mTermOut.write(data.getBytes());
			mTermOut.flush();
		} catch (IOException e) {
		}
	}

	private void createSubprocess(int[] processId) {
		ArrayList<String> args = parse(DEFAULT_SHELL);
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
					// do nothing
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
	public boolean onCheckIsTextEditor() {
		return true;
	}

	public void setTitleHeight(int height) {
		mTitleHeight = height;
	}

	@Override
	public InputConnection onCreateInputConnection(EditorInfo outAttrs) {
		outAttrs.inputType = mUseCookedIme ? EditorInfo.TYPE_CLASS_TEXT
				: EditorInfo.TYPE_NULL;
		return new BaseInputConnection(this, false) {

			@Override
			public boolean commitText(CharSequence text, int newCursorPosition) {
				sendText(text);
				return true;
			}

			@Override
			public boolean performEditorAction(int actionCode) {
				if (actionCode == EditorInfo.IME_ACTION_UNSPECIFIED) {
					// The "return" key has been pressed on the IME.
					sendText("\n");
					return true;
				}
				return false;
			}

			@Override
			public boolean sendKeyEvent(KeyEvent event) {
				if (event.getAction() == KeyEvent.ACTION_DOWN) {

					int keyCode = event.getKeyCode();
					if (keyCode >= 0 && keyCode < KEYCODE_CHARS.length()) {
						char c = KEYCODE_CHARS.charAt(keyCode);
						if (c > 0) {
							sendChar(c);
						}
					}
				}
				return true;
			}

			private final String KEYCODE_CHARS = "\000\000\000\000\000\000\000"
					+ "0123456789*#"
					+ "\000\000\000\000\000\000\000\000\000\000"
					+ "abcdefghijklmnopqrstuvwxyz,." + "\000\000\000\000"
					+ "\011 " // tab, space
					+ "\000\000\000" // sym .. envelope
					+ "\015\177" // enter, del
					+ "`-=[]\\;'/@" + "\000\000\000" + "+";

			@Override
			public boolean setComposingText(CharSequence text,
					int newCursorPosition) {
				return true;
			}

			@Override
			public boolean setSelection(int start, int end) {
				return true;
			}

			private void sendChar(int c) {
				try {
					mapAndSend(c);
				} catch (IOException ex) {

				}
			}

			private void sendText(CharSequence text) {
				int n = text.length();
				try {
					for (int i = 0; i < n; i++) {
						char c = text.charAt(i);
						mapAndSend(c);
					}
				} catch (IOException e) {
				}
			}

			private void mapAndSend(int c) throws IOException {
				mTermOut.write(mKeyListener.mapControlChar(c));
			}
		};
	}

	public boolean getKeypadApplicationMode() {
		return mEmulator.getKeypadApplicationMode();
	}

	private void commonConstructor() {
		mTextRenderer = null;
		mCursorPaint = new Paint();
		mCursorPaint.setARGB(255, 128, 128, 128);
		mBackgroundPaint = new Paint();
		mTopRow = 0;
		mLeftColumn = 0;
		mTitleHeight = 48;
		setVerticalScrollBarEnabled(true);
	}

	@Override
	protected int computeVerticalScrollRange() {
		return mTranscriptScreen.getActiveRows();
	}

	@Override
	protected int computeVerticalScrollExtent() {
		return mRows;
	}

	@Override
	protected int computeVerticalScrollOffset() {
		return mTranscriptScreen.getActiveRows() + mTopRow - mRows;
	}

	public void initialize(FileDescriptor termFd, FileOutputStream termOut) {
		mTermOut = termOut;
		mTermFd = termFd;
		mTextSize = 14;
		mForeground = 0xffffffff;
		mBackground = 0xff000000;
		updateText();
		mTermIn = new FileInputStream(mTermFd);
		mReceiveBuffer = new byte[4 * 1024];
		mByteQueue = new ByteQueue(4 * 1024);
	}

	public void append(byte[] buffer, int base, int length) {
		mEmulator.append(buffer, base, length);
		ensureCursorVisible();
		invalidate();
	}

	public void page(int delta) {
		mTopRow = Math.min(0, Math.max(
				-(mTranscriptScreen.getActiveTranscriptRows()), mTopRow + mRows
						* delta));
		invalidate();
	}

	public void pageHorizontal(int deltaColumns) {
		mLeftColumn = Math.max(0, Math.min(mLeftColumn + deltaColumns, mColumns
				- mVisibleColumns));
		invalidate();
	}

	public void setTextSize(int fontSize) {
		mTextSize = fontSize;
		updateText();
	}

	public void setCursorStyle(int blink) {
		if (blink != 0 && mCursorBlink == 0) {
			mHandler.postDelayed(mBlinkCursor, CURSOR_BLINK_PERIOD);
		} else if (blink == 0 && mCursorBlink != 0) {
			mHandler.removeCallbacks(mBlinkCursor);
		}
		mCursorBlink = blink;
	}

	public void setUseCookedIME(boolean useRawIME) {
		mUseCookedIme = useRawIME;
	}

	public boolean onSingleTapUp(MotionEvent e) {
		return true;
	}

	public void onLongPress(MotionEvent e) {
		showContextMenu();
	}

	public boolean onScroll(MotionEvent e1, MotionEvent e2, float distanceX,
			float distanceY) {
		distanceY += mScrollRemainder;
		int deltaRows = (int) (distanceY / mCharacterHeight);
		mScrollRemainder = distanceY - deltaRows * mCharacterHeight;
		mTopRow = Math.min(0, Math.max(
				-(mTranscriptScreen.getActiveTranscriptRows()), mTopRow
						+ deltaRows));
		invalidate();

		return true;
	}

	public void onSingleTapConfirmed(MotionEvent e) {
	}

	public boolean onJumpTapDown(MotionEvent e1, MotionEvent e2) {
		mTopRow = 0;
		invalidate();
		return true;
	}

	public boolean onJumpTapUp(MotionEvent e1, MotionEvent e2) {
		mTopRow = -mTranscriptScreen.getActiveTranscriptRows();
		invalidate();
		return true;
	}

	public boolean onFling(MotionEvent e1, MotionEvent e2, float velocityX,
			float velocityY) {
		mScrollRemainder = 0.0f;
		onScroll(e1, e2, 2 * velocityX, -2 * velocityY);
		return true;
	}

	public void onShowPress(MotionEvent e) {
	}

	public boolean onDown(MotionEvent e) {
		mScrollRemainder = 0.0f;
		return true;
	}

	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event) {

		if (handleControlKey(keyCode, true)) {
			return true;
		} else if (isSystemKey(keyCode, event)) {
			return super.onKeyDown(keyCode, event);
		} else if (handleDPad(keyCode, true)) {
			return true;
		}

		int letter = mKeyListener.keyDown(keyCode, event);

		if (letter >= 0) {
			try {
				mTermOut.write(letter);
			} catch (IOException e) {

			}
		}
		return true;
	}

	@Override
	public boolean onKeyUp(int keyCode, KeyEvent event) {

		if (handleControlKey(keyCode, false)) {
			return true;
		} else if (isSystemKey(keyCode, event)) {
			return super.onKeyUp(keyCode, event);
		} else if (handleDPad(keyCode, false)) {
			return true;
		}

		mKeyListener.keyUp(keyCode);
		return true;
	}

	private boolean handleControlKey(int keyCode, boolean down) {
		if (keyCode == mTerminalCallback.getControlKeyCode()) {
			mKeyListener.handleControlKey(down);
			return true;
		}
		return false;
	}

	private boolean handleDPad(int keyCode, boolean down) {
		if (keyCode < KeyEvent.KEYCODE_DPAD_UP
				|| keyCode > KeyEvent.KEYCODE_DPAD_CENTER) {
			return false;
		}

		if (down) {
			try {
				if (keyCode == KeyEvent.KEYCODE_DPAD_CENTER) {
					mTermOut.write('\r');
				} else {
					char code;
					switch (keyCode) {
					case KeyEvent.KEYCODE_DPAD_UP:
						code = 'A';
						break;
					case KeyEvent.KEYCODE_DPAD_DOWN:
						code = 'B';
						break;
					case KeyEvent.KEYCODE_DPAD_LEFT:
						code = 'D';
						break;
					default:
					case KeyEvent.KEYCODE_DPAD_RIGHT:
						code = 'C';
						break;
					}
					mTermOut.write(27); // ESC
					if (getKeypadApplicationMode()) {
						mTermOut.write('O');
					} else {
						mTermOut.write('[');
					}
					mTermOut.write(code);
				}
			} catch (IOException e) {
				// Ignore
			}
		}
		return true;
	}

	private boolean isSystemKey(int keyCode, KeyEvent event) {
		return event.isSystem();
	}

	private void updateText() {
		mTextRenderer = new PaintRenderer(mTextSize, mForeground, mBackground);
		mBackgroundPaint.setColor(mBackground);
		mCharacterWidth = mTextRenderer.getCharacterWidth();
		mCharacterHeight = mTextRenderer.getCharacterHeight();

		updateSize(true);
	}

	@Override
	protected void onSizeChanged(int w, int h, int oldw, int oldh) {
		boolean oldKnownSize = mKnownSize;
		if (!mKnownSize) {
			mKnownSize = true;
		}
		updateSize(false);
		if (!oldKnownSize) {
			mPollingThread = new Thread(new Runnable() {

				public void run() {
					try {
						while (true) {
							int read = mTermIn.read(mBuffer);
							mByteQueue.write(mBuffer, 0, read);
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
	}

	private void updateSize(int w, int h) {
		mColumns = Math.max(1, w / mCharacterWidth);
		mRows = Math.max(1, h / mCharacterHeight);
		mVisibleColumns = mVisibleWidth / mCharacterWidth;

		Exec.setPtyWindowSize(mTermFd, mRows, mColumns, w, h);

		if (mTranscriptScreen != null) {
			mEmulator.updateSize(mColumns, mRows);
		} else {
			mTranscriptScreen = new TranscriptScreen(mColumns, TRANSCRIPT_ROWS,
					mRows, 0, 7);
			mEmulator = new TerminalEmulator(mTranscriptScreen, mColumns,
					mRows, mTermOut);
		}

		mTopRow = 0;
		mLeftColumn = 0;

		invalidate();
	}

	void updateSize(boolean force) {
		if (mKnownSize) {
			getWindowVisibleDisplayFrame(mVisibleRect);
			int w = mVisibleRect.width();
			int h = mVisibleRect.height();
			if (force || w != mVisibleWidth
					|| (h - mTitleHeight) != mVisibleHeight) {
				mVisibleWidth = w;
				mVisibleHeight = h - mTitleHeight;
				updateSize(mVisibleWidth, mVisibleHeight);
			}
		}
	}

	private void update() {
		int bytesAvailable = mByteQueue.getBytesAvailable();
		int bytesToRead = Math.min(bytesAvailable, mReceiveBuffer.length);
		try {
			int bytesRead = mByteQueue.read(mReceiveBuffer, 0, bytesToRead);
			append(mReceiveBuffer, 0, bytesRead);
		} catch (InterruptedException e) {
		}
	}

	@Override
	protected void onDraw(Canvas canvas) {
		updateSize(false);
		int w = getWidth();
		int h = getHeight() - mTitleHeight;
		canvas.drawRect(0, 0, w, h, mBackgroundPaint);
		float x = -mLeftColumn * mCharacterWidth;
		float y = mCharacterHeight;
		int endLine = mTopRow + mRows;
		int cx = mEmulator.getCursorCol();
		int cy = mEmulator.getCursorRow();
		for (int i = mTopRow; i < endLine; i++) {
			int cursorX = -1;
			if (i == cy && mCursorVisible) {
				cursorX = cx;
			}
			mTranscriptScreen.drawText(i, canvas, x, y, mTextRenderer, cursorX);
			y += mCharacterHeight;
		}
	}

	private void ensureCursorVisible() {
		mTopRow = 0;
		if (mVisibleColumns > 0) {
			int cx = mEmulator.getCursorCol();
			int visibleCursorX = mEmulator.getCursorCol() - mLeftColumn;
			if (visibleCursorX < 0) {
				mLeftColumn = cx;
			} else if (visibleCursorX >= mVisibleColumns) {
				mLeftColumn = (cx - mVisibleColumns) + 1;
			}
		}
	}
}