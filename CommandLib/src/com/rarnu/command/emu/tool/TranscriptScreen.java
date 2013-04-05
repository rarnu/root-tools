package com.rarnu.command.emu.tool;

import android.graphics.Canvas;

import com.rarnu.command.emu.renderer.TextRenderer;

public class TranscriptScreen implements Screen {

	private int mColumns;

	private int mTotalRows;

	private int mActiveTranscriptRows;

	private int mHead;

	private int mActiveRows;

	private int mScreenRows;

	private char[] mData;

	private char[] mRowBuffer;

	private boolean[] mLineWrap;

	public TranscriptScreen(int columns, int totalRows, int screenRows,
			int foreColor, int backColor) {
		init(columns, totalRows, screenRows, foreColor, backColor);
	}

	private void init(int columns, int totalRows, int screenRows,
			int foreColor, int backColor) {
		mColumns = columns;
		mTotalRows = totalRows;
		mActiveTranscriptRows = 0;
		mHead = 0;
		mActiveRows = screenRows;
		mScreenRows = screenRows;
		int totalSize = columns * totalRows;
		mData = new char[totalSize];
		blockSet(0, 0, mColumns, mScreenRows, ' ', foreColor, backColor);
		mRowBuffer = new char[columns];
		mLineWrap = new boolean[totalRows];
		consistencyCheck();
	}

	private int externalToInternalRow(int row) {
		if (row < -mActiveTranscriptRows || row >= mScreenRows) {
			String errorMessage = "externalToInternalRow " + row + " "
					+ mActiveTranscriptRows + " " + mScreenRows;
			throw new IllegalArgumentException(errorMessage);
		}
		if (row >= 0) {
			return row; 
		}
		return mScreenRows
				+ ((mHead + mActiveTranscriptRows + row) % mActiveTranscriptRows);
	}

	private int getOffset(int externalLine) {
		return externalToInternalRow(externalLine) * mColumns;
	}

	private int getOffset(int x, int y) {
		return getOffset(y) + x;
	}

	public void setLineWrap(int row) {
		mLineWrap[externalToInternalRow(row)] = true;
	}

	public void set(int x, int y, byte b, int foreColor, int backColor) {
		mData[getOffset(x, y)] = encode(b, foreColor, backColor);
	}

	private char encode(int b, int foreColor, int backColor) {
		return (char) ((foreColor << 12) | (backColor << 8) | b);
	}

	public void scroll(int topMargin, int bottomMargin, int foreColor,
			int backColor) {

		if (topMargin > bottomMargin - 1) {
			throw new IllegalArgumentException();
		}

		if (topMargin > mScreenRows - 1) {
			throw new IllegalArgumentException();
		}

		if (bottomMargin > mScreenRows) {
			throw new IllegalArgumentException();
		}

		consistencyCheck();
		int expansionRows = Math.min(1, mTotalRows - mActiveRows);
		int rollRows = 1 - expansionRows;
		mActiveRows += expansionRows;
		mActiveTranscriptRows += expansionRows;
		if (mActiveTranscriptRows > 0) {
			mHead = (mHead + rollRows) % mActiveTranscriptRows;
		}
		consistencyCheck();

		// Block move the scroll line to the transcript
		int topOffset = getOffset(topMargin);
		int destOffset = getOffset(-1);
		System.arraycopy(mData, topOffset, mData, destOffset, mColumns);

		int topLine = externalToInternalRow(topMargin);
		int destLine = externalToInternalRow(-1);
		System.arraycopy(mLineWrap, topLine, mLineWrap, destLine, 1);

		// Block move the scrolled data up
		int numScrollChars = (bottomMargin - topMargin - 1) * mColumns;
		System.arraycopy(mData, topOffset + mColumns, mData, topOffset,
				numScrollChars);
		int numScrollLines = (bottomMargin - topMargin - 1);
		System.arraycopy(mLineWrap, topLine + 1, mLineWrap, topLine,
				numScrollLines);

		// Erase the bottom line of the scroll region
		blockSet(0, bottomMargin - 1, mColumns, 1, ' ', foreColor, backColor);
		mLineWrap[externalToInternalRow(bottomMargin - 1)] = false;
	}

	private void consistencyCheck() {
		checkPositive(mColumns);
		checkPositive(mTotalRows);
		checkRange(0, mActiveTranscriptRows, mTotalRows);
		if (mActiveTranscriptRows == 0) {
			checkEqual(mHead, 0);
		} else {
			checkRange(0, mHead, mActiveTranscriptRows - 1);
		}
		checkEqual(mScreenRows + mActiveTranscriptRows, mActiveRows);
		checkRange(0, mScreenRows, mTotalRows);

		checkEqual(mTotalRows, mLineWrap.length);
		checkEqual(mTotalRows * mColumns, mData.length);
		checkEqual(mColumns, mRowBuffer.length);
	}

	private void checkPositive(int n) {
		if (n < 0) {
			throw new IllegalArgumentException("checkPositive " + n);
		}
	}

	private void checkRange(int a, int b, int c) {
		if (a > b || b > c) {
			throw new IllegalArgumentException("checkRange " + a + " <= " + b
					+ " <= " + c);
		}
	}

	private void checkEqual(int a, int b) {
		if (a != b) {
			throw new IllegalArgumentException("checkEqual " + a + " == " + b);
		}
	}

	public void blockCopy(int sx, int sy, int w, int h, int dx, int dy) {
		if (sx < 0 || sx + w > mColumns || sy < 0 || sy + h > mScreenRows
				|| dx < 0 || dx + w > mColumns || dy < 0
				|| dy + h > mScreenRows) {
			throw new IllegalArgumentException();
		}
		if (sy <= dy) {
			// Move in increasing order
			for (int y = 0; y < h; y++) {
				int srcOffset = getOffset(sx, sy + y);
				int dstOffset = getOffset(dx, dy + y);
				System.arraycopy(mData, srcOffset, mData, dstOffset, w);
			}
		} else {
			// Move in decreasing order
			for (int y = 0; y < h; y++) {
				int y2 = h - (y + 1);
				int srcOffset = getOffset(sx, sy + y2);
				int dstOffset = getOffset(dx, dy + y2);
				System.arraycopy(mData, srcOffset, mData, dstOffset, w);
			}
		}
	}

	public void blockSet(int sx, int sy, int w, int h, int val, int foreColor,
			int backColor) {
		if (sx < 0 || sx + w > mColumns || sy < 0 || sy + h > mScreenRows) {
			throw new IllegalArgumentException();
		}
		char[] data = mData;
		char encodedVal = encode(val, foreColor, backColor);
		for (int y = 0; y < h; y++) {
			int offset = getOffset(sx, sy + y);
			for (int x = 0; x < w; x++) {
				data[offset + x] = encodedVal;
			}
		}
	}

	public final void drawText(int row, Canvas canvas, float x, float y,
			TextRenderer renderer, int cx) {

		// Out-of-bounds rows are blank.
		if (row < -mActiveTranscriptRows || row >= mScreenRows) {
			return;
		}

		int offset = getOffset(row);
		char[] rowBuffer = mRowBuffer;
		char[] data = mData;
		int columns = mColumns;
		int lastColors = 0;
		int lastRunStart = -1;
		final int CURSOR_MASK = 0x10000;
		for (int i = 0; i < columns; i++) {
			char c = data[offset + i];
			int colors = (char) (c & 0xff00);
			if (cx == i) {
				// Set cursor background color:
				colors |= CURSOR_MASK;
			}
			rowBuffer[i] = (char) (c & 0x00ff);
			if (colors != lastColors) {
				if (lastRunStart >= 0) {
					renderer.drawTextRun(canvas, x, y, lastRunStart, rowBuffer,
							lastRunStart, i - lastRunStart,
							(lastColors & CURSOR_MASK) != 0,
							0xf & (lastColors >> 12), 0xf & (lastColors >> 8));
				}
				lastColors = colors;
				lastRunStart = i;
			}
		}
		if (lastRunStart >= 0) {
			renderer.drawTextRun(canvas, x, y, lastRunStart, rowBuffer,
					lastRunStart, columns - lastRunStart,
					(lastColors & CURSOR_MASK) != 0, 0xf & (lastColors >> 12),
					0xf & (lastColors >> 8));
		}
	}

	public int getActiveRows() {
		return mActiveRows;
	}

	public int getActiveTranscriptRows() {
		return mActiveTranscriptRows;
	}

	public String getTranscriptText() {
		return internalGetTranscriptText(true);
	}

	private String internalGetTranscriptText(boolean stripColors) {
		StringBuilder builder = new StringBuilder();
		char[] rowBuffer = mRowBuffer;
		char[] data = mData;
		int columns = mColumns;
		for (int row = -mActiveTranscriptRows; row < mScreenRows; row++) {
			int offset = getOffset(row);
			int lastPrintingChar = -1;
			for (int column = 0; column < columns; column++) {
				char c = data[offset + column];
				if (stripColors) {
					c = (char) (c & 0xff);
				}
				if ((c & 0xff) != ' ') {
					lastPrintingChar = column;
				}
				rowBuffer[column] = c;
			}
			if (mLineWrap[externalToInternalRow(row)]) {
				builder.append(rowBuffer, 0, columns);
			} else {
				builder.append(rowBuffer, 0, lastPrintingChar + 1);
				builder.append('\n');
			}
		}
		return builder.toString();
	}

	public void resize(int columns, int rows, int foreColor, int backColor) {
		init(columns, mTotalRows, rows, foreColor, backColor);
	}
}