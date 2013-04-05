package com.rarnu.command.emu;

import java.io.FileOutputStream;
import java.io.IOException;

import com.rarnu.command.emu.tool.Screen;

public class TerminalEmulator {

	private int mCursorRow;
	private int mCursorCol;
	private int mRows;
	private int mColumns;
	private FileOutputStream mTermOut;
	private Screen mScreen;
	private int mArgIndex;
	private static final int MAX_ESCAPE_PARAMETERS = 16;
	private int[] mArgs = new int[MAX_ESCAPE_PARAMETERS];
	private static final int ESC_NONE = 0;
	private static final int ESC = 1;
	private static final int ESC_POUND = 2;
	private static final int ESC_SELECT_LEFT_PAREN = 3;
	private static final int ESC_SELECT_RIGHT_PAREN = 4;
	private static final int ESC_LEFT_SQUARE_BRACKET = 5;
	private static final int ESC_LEFT_SQUARE_BRACKET_QUESTION_MARK = 6;
	private boolean mContinueSequence;
	private int mEscapeState;
	private int mSavedCursorRow;
	private int mSavedCursorCol;
	private static final int K_132_COLUMN_MODE_MASK = 1 << 3;
	private static final int K_ORIGIN_MODE_MASK = 1 << 6;
	private int mDecFlags;
	private int mSavedDecFlags;
	private boolean mInsertMode;
	private boolean[] mTabStop;
	private int mTopMargin;
	private int mBottomMargin;
	private boolean mAboutToAutoWrap;
	private int mForeColor;
	private int mBackColor;
	private boolean mInverseColors;
	private boolean mbKeypadApplicationMode;

	public TerminalEmulator(Screen screen, int columns, int rows,
			FileOutputStream termOut) {
		mScreen = screen;
		mRows = rows;
		mColumns = columns;
		mTabStop = new boolean[mColumns];
		mTermOut = termOut;
		reset();
	}

	public void updateSize(int columns, int rows) {
		if (mRows == rows && mColumns == columns) {
			return;
		}
		if (columns <= 0) {
			throw new IllegalArgumentException("rows:" + columns);
		}

		if (rows <= 0) {
			throw new IllegalArgumentException("rows:" + rows);
		}

		String transcriptText = mScreen.getTranscriptText();

		mScreen.resize(columns, rows, mForeColor, mBackColor);

		if (mRows != rows) {
			mRows = rows;
			mTopMargin = 0;
			mBottomMargin = mRows;
		}
		if (mColumns != columns) {
			int oldColumns = mColumns;
			mColumns = columns;
			boolean[] oldTabStop = mTabStop;
			mTabStop = new boolean[mColumns];
			int toTransfer = Math.min(oldColumns, columns);
			System.arraycopy(oldTabStop, 0, mTabStop, 0, toTransfer);
			while (mCursorCol >= columns) {
				mCursorCol -= columns;
				mCursorRow = Math.min(mBottomMargin - 1, mCursorRow + 1);
			}
		}
		mCursorRow = 0;
		mCursorCol = 0;
		mAboutToAutoWrap = false;

		int end = transcriptText.length() - 1;
		while ((end >= 0) && transcriptText.charAt(end) == '\n') {
			end--;
		}
		for (int i = 0; i <= end; i++) {
			byte c = (byte) transcriptText.charAt(i);
			if (c == '\n') {
				setCursorCol(0);
				doLinefeed();
			} else {
				emit(c);
			}
		}
	}

	public final int getCursorRow() {
		return mCursorRow;
	}

	public final int getCursorCol() {
		return mCursorCol;
	}

	public final boolean getKeypadApplicationMode() {
		return mbKeypadApplicationMode;
	}

	private void setDefaultTabStops() {
		for (int i = 0; i < mColumns; i++) {
			mTabStop[i] = (i & 7) == 0 && i != 0;
		}
	}

	public void append(byte[] buffer, int base, int length) {
		for (int i = 0; i < length; i++) {
			byte b = buffer[base + i];
			try {
				process(b);
			} catch (Exception e) {

			}
		}
	}

	private void process(byte b) {
		switch (b) {
		case 0:
			break;
		case 7:
			break;
		case 8:
			setCursorCol(Math.max(0, mCursorCol - 1));
			break;
		case 9:
			setCursorCol(nextTabStop(mCursorCol));
			break;

		case 13:
			setCursorCol(0);
			break;

		case 10:
		case 11:
		case 12:
			doLinefeed();
			break;

		case 14:
			setAltCharSet(true);
			break;

		case 15:
			setAltCharSet(false);
			break;

		case 24:
		case 26:
			if (mEscapeState != ESC_NONE) {
				mEscapeState = ESC_NONE;
				emit((byte) 127);
			}
			break;

		case 27:
			startEscapeSequence(ESC);
			break;

		case (byte) 0x9b:
			startEscapeSequence(ESC_LEFT_SQUARE_BRACKET);
			break;

		default:
			mContinueSequence = false;
			switch (mEscapeState) {
			case ESC_NONE:
				if (b >= 32) {
					emit(b);
				}
				break;

			case ESC:
				doEsc(b);
				break;

			case ESC_POUND:
				doEscPound(b);
				break;

			case ESC_SELECT_LEFT_PAREN:
				doEscSelectLeftParen(b);
				break;

			case ESC_SELECT_RIGHT_PAREN:
				doEscSelectRightParen(b);
				break;

			case ESC_LEFT_SQUARE_BRACKET:
				doEscLeftSquareBracket(b);
				break;

			case ESC_LEFT_SQUARE_BRACKET_QUESTION_MARK:
				doEscLSBQuest(b);
				break;

			default:
				unknownSequence(b);
				break;
			}
			if (!mContinueSequence) {
				mEscapeState = ESC_NONE;
			}
			break;
		}
	}

	private void setAltCharSet(boolean alternateCharSet) {

	}

	private int nextTabStop(int cursorCol) {
		for (int i = cursorCol; i < mColumns; i++) {
			if (mTabStop[i]) {
				return i;
			}
		}
		return mColumns - 1;
	}

	private void doEscLSBQuest(byte b) {
		int mask = getDecFlagsMask(getArg0(0));
		switch (b) {
		case 'h':
			mDecFlags |= mask;
			break;

		case 'l':
			mDecFlags &= ~mask;
			break;

		case 'r':
			mDecFlags = (mDecFlags & ~mask) | (mSavedDecFlags & mask);
			break;

		case 's':
			mSavedDecFlags = (mSavedDecFlags & ~mask) | (mDecFlags & mask);
			break;

		default:
			parseArg(b);
			break;
		}

		if ((mask & K_132_COLUMN_MODE_MASK) != 0) {
			blockClear(0, 0, mColumns, mRows);
			setCursorRowCol(0, 0);
		}

		if ((mask & K_ORIGIN_MODE_MASK) != 0) {
			setCursorPosition(0, 0);
		}
	}

	private int getDecFlagsMask(int argument) {
		if (argument >= 1 && argument <= 9) {
			return (1 << argument);
		}

		return 0;
	}

	private void startEscapeSequence(int escapeState) {
		mEscapeState = escapeState;
		mArgIndex = 0;
		for (int j = 0; j < MAX_ESCAPE_PARAMETERS; j++) {
			mArgs[j] = -1;
		}
	}

	private void doLinefeed() {
		int newCursorRow = mCursorRow + 1;
		if (newCursorRow >= mBottomMargin) {
			scroll();
			newCursorRow = mBottomMargin - 1;
		}
		setCursorRow(newCursorRow);
	}

	private void continueSequence() {
		mContinueSequence = true;
	}

	private void continueSequence(int state) {
		mEscapeState = state;
		mContinueSequence = true;
	}

	private void doEscSelectLeftParen(byte b) {
		doSelectCharSet(true, b);
	}

	private void doEscSelectRightParen(byte b) {
		doSelectCharSet(false, b);
	}

	private void doSelectCharSet(boolean isG0CharSet, byte b) {
		switch (b) {
		case 'A':
		case 'B':
		case '0':
		case '1':
		case '2':
			break;
		default:
			unknownSequence(b);
		}
	}

	private void doEscPound(byte b) {
		switch (b) {
		case '8':
			mScreen.blockSet(0, 0, mColumns, mRows, 'E', getForeColor(),
					getBackColor());
			break;

		default:
			unknownSequence(b);
			break;
		}
	}

	private void doEsc(byte b) {
		switch (b) {
		case '#':
			continueSequence(ESC_POUND);
			break;

		case '(':
			continueSequence(ESC_SELECT_LEFT_PAREN);
			break;

		case ')':
			continueSequence(ESC_SELECT_RIGHT_PAREN);
			break;

		case '7':
			mSavedCursorRow = mCursorRow;
			mSavedCursorCol = mCursorCol;
			break;

		case '8':
			setCursorRowCol(mSavedCursorRow, mSavedCursorCol);
			break;

		case 'D':
			doLinefeed();
			break;

		case 'E':
			setCursorCol(0);
			doLinefeed();
			break;

		case 'F':
			setCursorRowCol(0, mBottomMargin - 1);
			break;

		case 'H':
			mTabStop[mCursorCol] = true;
			break;

		case 'M':
			if (mCursorRow == 0) {
				mScreen.blockCopy(0, mTopMargin + 1, mColumns, mBottomMargin
						- (mTopMargin + 1), 0, mTopMargin);
				blockClear(0, mBottomMargin - 1, mColumns);
			} else {
				mCursorRow--;
			}

			break;

		case 'N':
			unimplementedSequence(b);
			break;

		case '0':
			unimplementedSequence(b);
			break;

		case 'P':
			unimplementedSequence(b);
			break;

		case 'Z':
			sendDeviceAttributes();
			break;

		case '[':
			continueSequence(ESC_LEFT_SQUARE_BRACKET);
			break;

		case '=':
			mbKeypadApplicationMode = true;
			break;

		case '>':
			mbKeypadApplicationMode = false;
			break;

		default:
			unknownSequence(b);
			break;
		}
	}

	private void doEscLeftSquareBracket(byte b) {
		switch (b) {
		case '@': {
			int charsAfterCursor = mColumns - mCursorCol;
			int charsToInsert = Math.min(getArg0(1), charsAfterCursor);
			int charsToMove = charsAfterCursor - charsToInsert;
			mScreen.blockCopy(mCursorCol, mCursorRow, charsToMove, 1,
					mCursorCol + charsToInsert, mCursorRow);
			blockClear(mCursorCol, mCursorRow, charsToInsert);
		}
			break;

		case 'A':
			setCursorRow(Math.max(mTopMargin, mCursorRow - getArg0(1)));
			break;

		case 'B':
			setCursorRow(Math.min(mBottomMargin - 1, mCursorRow + getArg0(1)));
			break;

		case 'C':
			setCursorCol(Math.min(mColumns - 1, mCursorCol + getArg0(1)));
			break;

		case 'D':
			setCursorCol(Math.max(0, mCursorCol - getArg0(1)));
			break;

		case 'G':
			setCursorCol(Math.min(Math.max(1, getArg0(1)), mColumns) - 1);
			break;

		case 'H':
			setHorizontalVerticalPosition();
			break;

		case 'J':
			switch (getArg0(0)) {
			case 0:
				blockClear(mCursorCol, mCursorRow, mColumns - mCursorCol);
				blockClear(0, mCursorRow + 1, mColumns, mBottomMargin
						- (mCursorRow + 1));
				break;

			case 1:
				blockClear(0, mTopMargin, mColumns, mCursorRow - mTopMargin);
				blockClear(0, mCursorRow, mCursorCol + 1);
				break;

			case 2:
				blockClear(0, mTopMargin, mColumns, mBottomMargin - mTopMargin);
				break;

			default:
				unknownSequence(b);
				break;
			}
			break;

		case 'K':
			switch (getArg0(0)) {
			case 0:
				blockClear(mCursorCol, mCursorRow, mColumns - mCursorCol);
				break;

			case 1:
				blockClear(0, mCursorRow, mCursorCol + 1);
				break;

			case 2:
				blockClear(0, mCursorRow, mColumns);
				break;

			default:
				unknownSequence(b);
				break;
			}
			break;

		case 'L': {
			int linesAfterCursor = mBottomMargin - mCursorRow;
			int linesToInsert = Math.min(getArg0(1), linesAfterCursor);
			int linesToMove = linesAfterCursor - linesToInsert;
			mScreen.blockCopy(0, mCursorRow, mColumns, linesToMove, 0,
					mCursorRow + linesToInsert);
			blockClear(0, mCursorRow, mColumns, linesToInsert);
		}
			break;

		case 'M': {
			int linesAfterCursor = mBottomMargin - mCursorRow;
			int linesToDelete = Math.min(getArg0(1), linesAfterCursor);
			int linesToMove = linesAfterCursor - linesToDelete;
			mScreen.blockCopy(0, mCursorRow + linesToDelete, mColumns,
					linesToMove, 0, mCursorRow);
			blockClear(0, mCursorRow + linesToMove, mColumns, linesToDelete);
		}
			break;

		case 'P': {
			int charsAfterCursor = mColumns - mCursorCol;
			int charsToDelete = Math.min(getArg0(1), charsAfterCursor);
			int charsToMove = charsAfterCursor - charsToDelete;
			mScreen.blockCopy(mCursorCol + charsToDelete, mCursorRow,
					charsToMove, 1, mCursorCol, mCursorRow);
			blockClear(mCursorCol + charsToMove, mCursorRow, charsToDelete);
		}
			break;

		case 'T':
			unimplementedSequence(b);
			break;

		case '?':
			continueSequence(ESC_LEFT_SQUARE_BRACKET_QUESTION_MARK);
			break;

		case 'c':
			sendDeviceAttributes();
			break;

		case 'd':
			setCursorRow(Math.min(Math.max(1, getArg0(1)), mRows) - 1);
			break;

		case 'f':
			setHorizontalVerticalPosition();
			break;

		case 'g':
			switch (getArg0(0)) {
			case 0:
				mTabStop[mCursorCol] = false;
				break;

			case 3:
				for (int i = 0; i < mColumns; i++) {
					mTabStop[i] = false;
				}
				break;

			default:
				break;
			}
			break;

		case 'h':
			doSetMode(true);
			break;

		case 'l':
			doSetMode(false);
			break;

		case 'm':
			selectGraphicRendition();
			break;

		case 'r': {

			int top = Math.max(0, Math.min(getArg0(1) - 1, mRows - 2));
			int bottom = Math.max(top + 2, Math.min(getArg1(mRows), mRows));
			mTopMargin = top;
			mBottomMargin = bottom;
			setCursorRowCol(mTopMargin, 0);
		}
			break;

		default:
			parseArg(b);
			break;
		}
	}

	private void selectGraphicRendition() {
		for (int i = 0; i <= mArgIndex; i++) {
			int code = mArgs[i];
			if (code < 0) {
				if (mArgIndex > 0) {
					continue;
				} else {
					code = 0;
				}
			}
			if (code == 0) {
				mInverseColors = false;
				mForeColor = 7;
				mBackColor = 0;
			} else if (code == 1) {
				mForeColor |= 0x8;
			} else if (code == 4) {
				mBackColor |= 0x8;
			} else if (code == 7) {
				mInverseColors = true;
			} else if (code >= 30 && code <= 37) {
				mForeColor = (mForeColor & 0x8) | (code - 30);
			} else if (code >= 40 && code <= 47) {
				mBackColor = (mBackColor & 0x8) | (code - 40);
			}
		}
	}

	private void blockClear(int sx, int sy, int w) {
		blockClear(sx, sy, w, 1);
	}

	private void blockClear(int sx, int sy, int w, int h) {
		mScreen.blockSet(sx, sy, w, h, ' ', getForeColor(), getBackColor());
	}

	private int getForeColor() {
		return mInverseColors ? ((mBackColor & 0x7) | (mForeColor & 0x8))
				: mForeColor;
	}

	private int getBackColor() {
		return mInverseColors ? ((mForeColor & 0x7) | (mBackColor & 0x8))
				: mBackColor;
	}

	private void doSetMode(boolean newValue) {
		int modeBit = getArg0(0);
		switch (modeBit) {
		case 4:
			mInsertMode = newValue;
			break;

		case 20:
			break;

		default:
			unknownParameter(modeBit);
			break;
		}
	}

	private void setHorizontalVerticalPosition() {
		setCursorPosition(getArg1(1) - 1, getArg0(1) - 1);
	}

	private void setCursorPosition(int x, int y) {
		int effectiveTopMargin = 0;
		int effectiveBottomMargin = mRows;
		if ((mDecFlags & K_ORIGIN_MODE_MASK) != 0) {
			effectiveTopMargin = mTopMargin;
			effectiveBottomMargin = mBottomMargin;
		}
		int newRow = Math.max(effectiveTopMargin,
				Math.min(effectiveTopMargin + y, effectiveBottomMargin - 1));
		int newCol = Math.max(0, Math.min(x, mColumns - 1));
		setCursorRowCol(newRow, newCol);
	}

	private void sendDeviceAttributes() {

		byte[] attributes = { (byte) 27, (byte) '[', (byte) '?', (byte) '1',
				(byte) ';', (byte) '2', (byte) 'c'

		};

		write(attributes);
	}

	private void write(byte[] data) {
		try {
			mTermOut.write(data);
			mTermOut.flush();
		} catch (IOException e) {

		}
	}

	private void scroll() {
		mScreen.scroll(mTopMargin, mBottomMargin, getForeColor(),
				getBackColor());
	}

	private void parseArg(byte b) {
		if (b >= '0' && b <= '9') {
			if (mArgIndex < mArgs.length) {
				int oldValue = mArgs[mArgIndex];
				int thisDigit = b - '0';
				int value;
				if (oldValue >= 0) {
					value = oldValue * 10 + thisDigit;
				} else {
					value = thisDigit;
				}
				mArgs[mArgIndex] = value;
			}
			continueSequence();
		} else if (b == ';') {
			if (mArgIndex < mArgs.length) {
				mArgIndex++;
			}
			continueSequence();
		} else {
			unknownSequence(b);
		}
	}

	private int getArg0(int defaultValue) {
		return getArg(0, defaultValue);
	}

	private int getArg1(int defaultValue) {
		return getArg(1, defaultValue);
	}

	private int getArg(int index, int defaultValue) {
		int result = mArgs[index];
		if (result < 0) {
			result = defaultValue;
		}
		return result;
	}

	private void unimplementedSequence(byte b) {
		finishSequence();
	}

	private void unknownSequence(byte b) {

		finishSequence();
	}

	private void unknownParameter(int parameter) {

	}

	private void finishSequence() {
		mEscapeState = ESC_NONE;
	}

	private boolean autoWrapEnabled() {
		return true;

	}

	private void emit(byte b) {
		boolean autoWrap = autoWrapEnabled();
		if (autoWrap) {
			if (mCursorCol == mColumns - 1 && mAboutToAutoWrap) {
				mScreen.setLineWrap(mCursorRow);
				mCursorCol = 0;
				if (mCursorRow + 1 < mBottomMargin) {
					mCursorRow++;
				} else {
					scroll();
				}
			}
		}

		if (mInsertMode) {
			int destCol = mCursorCol + 1;
			if (destCol < mColumns) {
				mScreen.blockCopy(mCursorCol, mCursorRow, mColumns - destCol,
						1, destCol, mCursorRow);
			}
		}

		mScreen.set(mCursorCol, mCursorRow, b, getForeColor(), getBackColor());

		if (autoWrap) {
			mAboutToAutoWrap = (mCursorCol == mColumns - 1);
		}

		mCursorCol = Math.min(mCursorCol + 1, mColumns - 1);
	}

	private void setCursorRow(int row) {
		mCursorRow = row;
		mAboutToAutoWrap = false;
	}

	private void setCursorCol(int col) {
		mCursorCol = col;
		mAboutToAutoWrap = false;
	}

	private void setCursorRowCol(int row, int col) {
		mCursorRow = Math.min(row, mRows - 1);
		mCursorCol = Math.min(col, mColumns - 1);
		mAboutToAutoWrap = false;
	}

	public void reset() {
		mCursorRow = 0;
		mCursorCol = 0;
		mArgIndex = 0;
		mContinueSequence = false;
		mEscapeState = ESC_NONE;
		mSavedCursorRow = 0;
		mSavedCursorCol = 0;
		mDecFlags = 0;
		mSavedDecFlags = 0;
		mInsertMode = false;
		mTopMargin = 0;
		mBottomMargin = mRows;
		mAboutToAutoWrap = false;
		mForeColor = 7;
		mBackColor = 0;
		mInverseColors = false;
		mbKeypadApplicationMode = false;
		setDefaultTabStops();
		blockClear(0, 0, mColumns, mRows);
	}

	public String getTranscriptText() {
		return mScreen.getTranscriptText();
	}
}