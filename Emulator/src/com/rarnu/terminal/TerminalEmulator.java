package com.rarnu.terminal;

import com.rarnu.terminal.callback.UpdateCallback;
import com.rarnu.terminal.renderer.TextStyle;
import com.rarnu.terminal.screen.Screen;
import com.rarnu.terminal.screen.UnicodeTranscript;
import com.rarnu.terminal.session.TermSession;
import com.rarnu.terminal.utils.ColorScheme;
import com.rarnu.terminal.utils.GrowableIntArray;

import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CodingErrorAction;

public class TerminalEmulator {

    private static final int MAX_ESCAPE_PARAMETERS = 16;
    private static final int MAX_OSC_STRING_LENGTH = 512;
    private static final int ESC_NONE = 0;
    private static final int ESC = 1;
    private static final int ESC_POUND = 2;
    private static final int ESC_SELECT_LEFT_PAREN = 3;
    private static final int ESC_SELECT_RIGHT_PAREN = 4;
    private static final int ESC_LEFT_SQUARE_BRACKET = 5;
    private static final int ESC_LEFT_SQUARE_BRACKET_QUESTION_MARK = 6;
    private static final int ESC_PERCENT = 7;
    private static final int ESC_RIGHT_SQUARE_BRACKET = 8;
    private static final int ESC_RIGHT_SQUARE_BRACKET_ESC = 9;
    private static final int K_132_COLUMN_MODE_MASK = 1 << 3;
    private static final int K_REVERSE_VIDEO_MASK = 1 << 5;
    private static final int K_ORIGIN_MODE_MASK = 1 << 6;
    private static final int K_WRAPAROUND_MODE_MASK = 1 << 7;
    private static final int K_SHOW_CURSOR_MASK = 1 << 25;
    private static final int K_DECSC_DECRC_MASK = K_ORIGIN_MODE_MASK | K_WRAPAROUND_MODE_MASK;
    private final static int CHAR_SET_UK = 0;
    private final static int CHAR_SET_ASCII = 1;
    private final static int CHAR_SET_SPECIAL_GRAPHICS = 2;
    private final static int CHAR_SET_ALT_STANDARD = 3;
    private final static int CHAR_SET_ALT_SPECIAL_GRAPICS = 4;
    private static final char[] mSpecialGraphicsCharMap = new char[128];

    static {
        for (char i = 0; i < 128; ++i) {
            mSpecialGraphicsCharMap[i] = i;
        }
        mSpecialGraphicsCharMap['_'] = ' ';
        mSpecialGraphicsCharMap['b'] = 0x2409;
        mSpecialGraphicsCharMap['c'] = 0x240C;
        mSpecialGraphicsCharMap['d'] = 0x240D;
        mSpecialGraphicsCharMap['e'] = 0x240A;
        mSpecialGraphicsCharMap['h'] = 0x2424;
        mSpecialGraphicsCharMap['i'] = 0x240B;
        mSpecialGraphicsCharMap['}'] = 0x00A3;
        mSpecialGraphicsCharMap['f'] = 0x00B0;
        mSpecialGraphicsCharMap['`'] = 0x2B25;
        mSpecialGraphicsCharMap['~'] = 0x2022;
        mSpecialGraphicsCharMap['y'] = 0x2264;
        mSpecialGraphicsCharMap['|'] = 0x2260;
        mSpecialGraphicsCharMap['z'] = 0x2265;
        mSpecialGraphicsCharMap['g'] = 0x00B1;
        mSpecialGraphicsCharMap['{'] = 0x03C0;
        mSpecialGraphicsCharMap['.'] = 0x25BC;
        mSpecialGraphicsCharMap[','] = 0x25C0;
        mSpecialGraphicsCharMap['+'] = 0x25B6;
        mSpecialGraphicsCharMap['-'] = 0x25B2;
        mSpecialGraphicsCharMap['h'] = '#';
        mSpecialGraphicsCharMap['a'] = 0x2592;
        mSpecialGraphicsCharMap['0'] = 0x2588;
        mSpecialGraphicsCharMap['q'] = 0x2500;
        mSpecialGraphicsCharMap['x'] = 0x2502;
        mSpecialGraphicsCharMap['m'] = 0x2514;
        mSpecialGraphicsCharMap['j'] = 0x2518;
        mSpecialGraphicsCharMap['l'] = 0x250C;
        mSpecialGraphicsCharMap['k'] = 0x2510;
        mSpecialGraphicsCharMap['w'] = 0x252C;
        mSpecialGraphicsCharMap['u'] = 0x2524;
        mSpecialGraphicsCharMap['t'] = 0x251C;
        mSpecialGraphicsCharMap['v'] = 0x2534;
        mSpecialGraphicsCharMap['n'] = 0x253C;
        mSpecialGraphicsCharMap['o'] = 0x23BA;
        mSpecialGraphicsCharMap['p'] = 0x23BB;
        mSpecialGraphicsCharMap['r'] = 0x23BC;
        mSpecialGraphicsCharMap['s'] = 0x23BD;
    }

    private static final int UNICODE_REPLACEMENT_CHAR = 0xfffd;
    private final static boolean DEFAULT_TO_AUTOWRAP_ENABLED = true;
    private int mCursorRow;
    private int mCursorCol;
    private int mRows;
    private int mColumns;
    private Screen mScreen;
    private TermSession mSession;
    private int mArgIndex;
    private int[] mArgs = new int[MAX_ESCAPE_PARAMETERS];
    private byte[] mOSCArg = new byte[MAX_OSC_STRING_LENGTH];
    private int mOSCArgLength;
    private int mOSCArgTokenizerIndex;
    private boolean mContinueSequence;
    private int mEscapeState;
    private int mSavedCursorRow;
    private int mSavedCursorCol;
    private int mSavedEffect;
    private int mSavedDecFlags_DECSC_DECRC;
    private int mDecFlags;
    private int mSavedDecFlags;
    private boolean mInsertMode;
    private boolean[] mTabStop;
    private int mTopMargin;
    private int mBottomMargin;
    private boolean mAboutToAutoWrap;
    private int mLastEmittedCharWidth = 0;
    private boolean mJustWrapped = false;
    private int mForeColor;
    private int mDefaultForeColor;
    private int mBackColor;
    private int mDefaultBackColor;
    private int mEffect;
    private boolean mbKeypadApplicationMode;
    private boolean mAlternateCharSet;
    private int[] mCharSet = new int[2];
    private boolean mUseAlternateCharSet;
    private int mScrollCounter = 0;
    private boolean mDefaultUTF8Mode = false;
    private boolean mUTF8Mode = false;
    private boolean mUTF8EscapeUsed = false;
    private int mUTF8ToFollow = 0;
    private ByteBuffer mUTF8ByteBuffer;
    private CharBuffer mInputCharBuffer;
    private CharsetDecoder mUTF8Decoder;
    private UpdateCallback mUTF8ModeNotify;

    public TerminalEmulator(TermSession session, Screen screen, int columns, int rows, ColorScheme scheme) {
        mSession = session;
        mScreen = screen;
        mRows = rows;
        mColumns = columns;
        mTabStop = new boolean[mColumns];
        setColorScheme(scheme);
        mUTF8ByteBuffer = ByteBuffer.allocate(4);
        mInputCharBuffer = CharBuffer.allocate(2);
        mUTF8Decoder = Charset.forName("UTF-8").newDecoder();
        mUTF8Decoder.onMalformedInput(CodingErrorAction.REPLACE);
        mUTF8Decoder.onUnmappableCharacter(CodingErrorAction.REPLACE);
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

        int[] cursor = {mCursorCol, mCursorRow};
        boolean fastResize = mScreen.fastResize(columns, rows, cursor);

        GrowableIntArray cursorColor = null;
        String charAtCursor = null;
        GrowableIntArray colors = null;
        String transcriptText = null;
        if (!fastResize) {
            cursorColor = new GrowableIntArray(1);
            charAtCursor = mScreen.getSelectedText(cursorColor, mCursorCol, mCursorRow, mCursorCol, mCursorRow);
            mScreen.set(mCursorCol, mCursorRow, 27, 0);

            colors = new GrowableIntArray(1024);
            transcriptText = mScreen.getTranscriptText(colors);

            mScreen.resize(columns, rows, getStyle());
        }

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
        }

        if (fastResize) {
            if (cursor[0] >= 0 && cursor[1] >= 0) {
                mCursorCol = cursor[0];
                mCursorRow = cursor[1];
            } else {
                mCursorCol = 0;
                mCursorRow = 0;
            }

            return;
        }

        mCursorRow = 0;
        mCursorCol = 0;
        mAboutToAutoWrap = false;

        int newCursorRow = -1;
        int newCursorCol = -1;
        int newCursorTranscriptPos = -1;
        int end = transcriptText.length() - 1;
        while ((end >= 0) && transcriptText.charAt(end) == '\n') {
            end--;
        }
        char c, cLow;
        int colorOffset = 0;
        for (int i = 0; i <= end; i++) {
            c = transcriptText.charAt(i);
            int style = colors.at(i - colorOffset);
            if (Character.isHighSurrogate(c)) {
                cLow = transcriptText.charAt(++i);
                emit(Character.toCodePoint(c, cLow), style);
                ++colorOffset;
            } else if (c == '\n') {
                setCursorCol(0);
                doLinefeed();
            } else if (c == 27) {
                newCursorRow = mCursorRow;
                newCursorCol = mCursorCol;
                newCursorTranscriptPos = mScreen.getActiveRows();
                if (charAtCursor != null && charAtCursor.length() > 0) {
                    int encodedCursorColor = cursorColor.at(0);
                    emit(charAtCursor.toCharArray(), 0, charAtCursor.length(), encodedCursorColor);
                }
            } else {
                emit(c, style);
            }
        }

        if (newCursorRow != -1 && newCursorCol != -1) {
            mCursorRow = newCursorRow;
            mCursorCol = newCursorCol;

            int scrollCount = mScreen.getActiveRows() - newCursorTranscriptPos;
            if (scrollCount > 0 && scrollCount <= newCursorRow) {
                mCursorRow -= scrollCount;
            } else if (scrollCount > newCursorRow) {
                mCursorRow = 0;
                mCursorCol = 0;
            }
        }
    }

    public final int getCursorRow() {
        return mCursorRow;
    }

    private void setCursorRow(int row) {
        mCursorRow = row;
        mAboutToAutoWrap = false;
    }

    public final int getCursorCol() {
        return mCursorCol;
    }

    private void setCursorCol(int col) {
        mCursorCol = col;
        mAboutToAutoWrap = false;
    }

    public final boolean getReverseVideo() {
        return (mDecFlags & K_REVERSE_VIDEO_MASK) != 0;
    }

    public final boolean getShowCursor() {
        return (mDecFlags & K_SHOW_CURSOR_MASK) != 0;
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
        process(b, true);
    }

    private void process(byte b, boolean doUTF8) {
        if (doUTF8 && mUTF8Mode && handleUTF8Sequence(b)) {
            return;
        }

        if ((b & 0x80) == 0x80 && (b & 0x7f) <= 0x1f) {
            process((byte) 27, false);
            process((byte) ((b & 0x7f) + 0x40), false);
            return;
        }

        switch (b) {
            case 0:
                break;

            case 7:
                if (mEscapeState == ESC_RIGHT_SQUARE_BRACKET) {
                    doEscRightSquareBracket(b);
                }
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
                if (mEscapeState != ESC_RIGHT_SQUARE_BRACKET) {
                    startEscapeSequence(ESC);
                } else {
                    doEscRightSquareBracket(b);
                }
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

                    case ESC_PERCENT:
                        doEscPercent(b);
                        break;

                    case ESC_RIGHT_SQUARE_BRACKET:
                        doEscRightSquareBracket(b);
                        break;

                    case ESC_RIGHT_SQUARE_BRACKET_ESC:
                        doEscRightSquareBracketEsc(b);
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

    private boolean handleUTF8Sequence(byte b) {
        if (mUTF8ToFollow == 0 && (b & 0x80) == 0) {
            return false;
        }

        if (mUTF8ToFollow > 0) {
            if ((b & 0xc0) != 0x80) {

                mUTF8ToFollow = 0;
                mUTF8ByteBuffer.clear();
                emit(UNICODE_REPLACEMENT_CHAR);
                return true;
            }

            mUTF8ByteBuffer.put(b);
            if (--mUTF8ToFollow == 0) {

                ByteBuffer byteBuf = mUTF8ByteBuffer;
                CharBuffer charBuf = mInputCharBuffer;
                CharsetDecoder decoder = mUTF8Decoder;

                byteBuf.rewind();
                decoder.reset();
                decoder.decode(byteBuf, charBuf, true);
                decoder.flush(charBuf);

                char[] chars = charBuf.array();
                if (chars[0] >= 0x80 && chars[0] <= 0x9f) {

                    process((byte) chars[0], false);
                } else {
                    emit(chars);
                }

                byteBuf.clear();
                charBuf.clear();
            }
        } else {
            if ((b & 0xe0) == 0xc0) {
                mUTF8ToFollow = 1;
            } else if ((b & 0xf0) == 0xe0) {
                mUTF8ToFollow = 2;
            } else if ((b & 0xf8) == 0xf0) {
                mUTF8ToFollow = 3;
            } else {

                emit(UNICODE_REPLACEMENT_CHAR);
                return true;
            }

            mUTF8ByteBuffer.put(b);
        }

        return true;
    }

    private void setAltCharSet(boolean alternateCharSet) {
        mAlternateCharSet = alternateCharSet;
        computeEffectiveCharSet();
    }

    private void computeEffectiveCharSet() {
        int charSet = mCharSet[mAlternateCharSet ? 1 : 0];
        mUseAlternateCharSet = charSet == CHAR_SET_SPECIAL_GRAPHICS;
    }

    private int nextTabStop(int cursorCol) {
        for (int i = cursorCol + 1; i < mColumns; i++) {
            if (mTabStop[i]) {
                return i;
            }
        }
        return mColumns - 1;
    }

    private int prevTabStop(int cursorCol) {
        for (int i = cursorCol - 1; i >= 0; i--) {
            if (mTabStop[i]) {
                return i;
            }
        }
        return 0;
    }

    private void doEscPercent(byte b) {
        switch (b) {
            case '@':
                setUTF8Mode(false);
                mUTF8EscapeUsed = true;
                break;
            case 'G':
                setUTF8Mode(true);
                mUTF8EscapeUsed = true;
                break;
            default:
                break;
        }
    }

    private void doEscLSBQuest(byte b) {
        int mask = getDecFlagsMask(getArg0(0));
        int oldFlags = mDecFlags;
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

        int newlySetFlags = (~oldFlags) & mDecFlags;
        int changedFlags = oldFlags ^ mDecFlags;

        if ((changedFlags & K_132_COLUMN_MODE_MASK) != 0) {

            blockClear(0, 0, mColumns, mRows);
            setCursorRowCol(0, 0);
        }

        if ((newlySetFlags & K_ORIGIN_MODE_MASK) != 0) {
            setCursorPosition(0, 0);
        }
    }

    private int getDecFlagsMask(int argument) {
        if (argument >= 1 && argument <= 32) {
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
        doSelectCharSet(0, b);
    }

    private void doEscSelectRightParen(byte b) {
        doSelectCharSet(1, b);
    }

    private void doSelectCharSet(int charSetIndex, byte b) {
        int charSet;
        switch (b) {
            case 'A':
                charSet = CHAR_SET_UK;
                break;
            case 'B':
                charSet = CHAR_SET_ASCII;
                break;
            case '0':
                charSet = CHAR_SET_SPECIAL_GRAPHICS;
                break;
            case '1':
                charSet = CHAR_SET_ALT_STANDARD;
                break;
            case '2':
                charSet = CHAR_SET_ALT_SPECIAL_GRAPICS;
                break;
            default:
                unknownSequence(b);
                return;
        }
        mCharSet[charSetIndex] = charSet;
        computeEffectiveCharSet();
    }

    private void doEscPound(byte b) {
        switch (b) {
            case '8':
                mScreen.blockSet(0, 0, mColumns, mRows, 'E', getStyle());
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
                mSavedEffect = mEffect;
                mSavedDecFlags_DECSC_DECRC = mDecFlags & K_DECSC_DECRC_MASK;
                break;

            case '8':
                setCursorRowCol(mSavedCursorRow, mSavedCursorCol);
                mEffect = mSavedEffect;
                mDecFlags = (mDecFlags & ~K_DECSC_DECRC_MASK) | mSavedDecFlags_DECSC_DECRC;
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
                if (mCursorRow <= mTopMargin) {
                    mScreen.blockCopy(0, mTopMargin, mColumns, mBottomMargin - (mTopMargin + 1), 0, mTopMargin + 1);
                    blockClear(0, mTopMargin, mColumns);
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

            case ']':
                startCollectingOSCArgs();
                continueSequence(ESC_RIGHT_SQUARE_BRACKET);
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
                mScreen.blockCopy(mCursorCol, mCursorRow, charsToMove, 1, mCursorCol + charsToInsert, mCursorRow);
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
                        blockClear(0, mCursorRow + 1, mColumns, mRows - (mCursorRow + 1));
                        break;

                    case 1:
                        blockClear(0, 0, mColumns, mCursorRow);
                        blockClear(0, mCursorRow, mCursorCol + 1);
                        break;

                    case 2:
                        blockClear(0, 0, mColumns, mRows);
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
                mScreen.blockCopy(0, mCursorRow, mColumns, linesToMove, 0, mCursorRow + linesToInsert);
                blockClear(0, mCursorRow, mColumns, linesToInsert);
            }
            break;

            case 'M': {
                int linesAfterCursor = mBottomMargin - mCursorRow;
                int linesToDelete = Math.min(getArg0(1), linesAfterCursor);
                int linesToMove = linesAfterCursor - linesToDelete;
                mScreen.blockCopy(0, mCursorRow + linesToDelete, mColumns, linesToMove, 0, mCursorRow);
                blockClear(0, mCursorRow + linesToMove, mColumns, linesToDelete);
            }
            break;

            case 'P': {
                int charsAfterCursor = mColumns - mCursorCol;
                int charsToDelete = Math.min(getArg0(1), charsAfterCursor);
                int charsToMove = charsAfterCursor - charsToDelete;
                mScreen.blockCopy(mCursorCol + charsToDelete, mCursorRow, charsToMove, 1, mCursorCol, mCursorRow);
                blockClear(mCursorCol + charsToMove, mCursorRow, charsToDelete);
            }
            break;

            case 'T':
                unimplementedSequence(b);
                break;

            case 'X':
                blockClear(mCursorCol, mCursorRow, getArg0(0));
                break;

            case 'Z':
                setCursorCol(prevTabStop(mCursorCol));
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
                mForeColor = mDefaultForeColor;
                mBackColor = mDefaultBackColor;
                mEffect = TextStyle.fxNormal;
            } else if (code == 1) {
                mEffect |= TextStyle.fxBold;
            } else if (code == 3) {
                mEffect |= TextStyle.fxItalic;
            } else if (code == 4) {
                mEffect |= TextStyle.fxUnderline;
            } else if (code == 5) {
                mEffect |= TextStyle.fxBlink;
            } else if (code == 7) {
                mEffect |= TextStyle.fxInverse;
            } else if (code == 8) {
                mEffect |= TextStyle.fxInvisible;
            } else if (code == 10) {
                setAltCharSet(false);
            } else if (code == 11) {
                setAltCharSet(true);
            } else if (code == 22) {
                mEffect &= ~TextStyle.fxBold;
            } else if (code == 23) {
                mEffect &= ~TextStyle.fxItalic;
            } else if (code == 24) {
                mEffect &= ~TextStyle.fxUnderline;
            } else if (code == 25) {
                mEffect &= ~TextStyle.fxBlink;
            } else if (code == 27) {
                mEffect &= ~TextStyle.fxInverse;
            } else if (code == 28) {
                mEffect &= ~TextStyle.fxInvisible;
            } else if (code >= 30 && code <= 37) {
                mForeColor = code - 30;
            } else if (code == 38 && i + 2 <= mArgIndex && mArgs[i + 1] == 5) {
                int color = mArgs[i + 2];
                if (checkColor(color)) {
                    mForeColor = color;
                }
                i += 2;
            } else if (code == 39) {
                mForeColor = mDefaultForeColor;
            } else if (code >= 40 && code <= 47) {
                mBackColor = code - 40;
            } else if (code == 48 && i + 2 <= mArgIndex && mArgs[i + 1] == 5) {
                mBackColor = mArgs[i + 2];
                int color = mArgs[i + 2];
                if (checkColor(color)) {
                    mBackColor = color;
                }
                i += 2;
            } else if (code == 49) {
                mBackColor = mDefaultBackColor;
            } else if (code >= 90 && code <= 97) {
                mForeColor = code - 90 + 8;
            } else if (code >= 100 && code <= 107) {
                mBackColor = code - 100 + 8;
            }
        }
    }

    private boolean checkColor(int color) {
        boolean result = isValidColor(color);
        return result;
    }

    private boolean isValidColor(int color) {
        return color >= 0 && color < TextStyle.ciColorLength;
    }

    private void doEscRightSquareBracket(byte b) {
        switch (b) {
            case 0x7:
                doOSC();
                break;
            case 0x1b:
                continueSequence(ESC_RIGHT_SQUARE_BRACKET_ESC);
                break;
            default:
                collectOSCArgs(b);
                break;
        }
    }

    private void doEscRightSquareBracketEsc(byte b) {
        switch (b) {
            case '\\':
                doOSC();
                break;

            default:

                collectOSCArgs((byte) 0x1b);
                collectOSCArgs(b);
                continueSequence(ESC_RIGHT_SQUARE_BRACKET);
                break;
        }
    }

    private void doOSC() {
        startTokenizingOSC();
        int ps = nextOSCInt(';');
        switch (ps) {
            case 0:
            case 1:
            case 2:
                changeTitle(ps, nextOSCString(-1));
                break;
            default:
                unknownParameter(ps);
                break;
        }
        finishSequence();
    }

    private void changeTitle(int parameter, String title) {
        if (parameter == 0 || parameter == 2) {
            mSession.setTitle(title);
        }
    }

    private void blockClear(int sx, int sy, int w) {
        blockClear(sx, sy, w, 1);
    }

    private void blockClear(int sx, int sy, int w, int h) {
        mScreen.blockSet(sx, sy, w, h, ' ', getStyle());
    }

    private int getForeColor() {
        return mForeColor;
    }

    private int getBackColor() {
        return mBackColor;
    }

    private int getEffect() {
        return mEffect;
    }

    private int getStyle() {
        return TextStyle.encode(getForeColor(), getBackColor(), getEffect());
    }

    private void doSetMode(boolean newValue) {
        int modeBit = getArg0(0);
        switch (modeBit) {
            case 4:
                mInsertMode = newValue;
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
        int newRow = Math.max(effectiveTopMargin, Math.min(effectiveTopMargin + y, effectiveBottomMargin - 1));
        int newCol = Math.max(0, Math.min(x, mColumns - 1));
        setCursorRowCol(newRow, newCol);
    }

    private void sendDeviceAttributes() {

        byte[] attributes = {(byte) 27, (byte) '[', (byte) '?', (byte) '1',
                (byte) ';', (byte) '2', (byte) 'c'};

        mSession.write(attributes, 0, attributes.length);
    }

    private void scroll() {

        mScrollCounter++;
        mScreen.scroll(mTopMargin, mBottomMargin, getStyle());
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
        return getArg(0, defaultValue, true);
    }

    private int getArg1(int defaultValue) {
        return getArg(1, defaultValue, true);
    }

    private int getArg(int index, int defaultValue, boolean treatZeroAsDefault) {
        int result = mArgs[index];
        if (result < 0 || (result == 0 && treatZeroAsDefault)) {
            result = defaultValue;
        }
        return result;
    }

    private void startCollectingOSCArgs() {
        mOSCArgLength = 0;
    }

    private void collectOSCArgs(byte b) {
        if (mOSCArgLength < MAX_OSC_STRING_LENGTH) {
            mOSCArg[mOSCArgLength++] = b;
            continueSequence();
        } else {
            unknownSequence(b);
        }
    }

    private void startTokenizingOSC() {
        mOSCArgTokenizerIndex = 0;
    }

    private String nextOSCString(int delimiter) {
        int start = mOSCArgTokenizerIndex;
        int end = start;
        while (mOSCArgTokenizerIndex < mOSCArgLength) {
            byte b = mOSCArg[mOSCArgTokenizerIndex++];
            if ((int) b == delimiter) {
                break;
            }
            end++;
        }
        if (start == end) {
            return "";
        }
        try {
            return new String(mOSCArg, start, end - start, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            return new String(mOSCArg, start, end - start);
        }
    }

    private int nextOSCInt(int delimiter) {
        int value = -1;
        while (mOSCArgTokenizerIndex < mOSCArgLength) {
            byte b = mOSCArg[mOSCArgTokenizerIndex++];
            if ((int) b == delimiter) {
                break;
            } else if (b >= '0' && b <= '9') {
                if (value < 0) {
                    value = 0;
                }
                value = value * 10 + b - '0';
            } else {
                unknownSequence(b);
            }
        }
        return value;
    }

    private void unimplementedSequence(byte b) {
        logError();
        finishSequence();
    }

    private void unknownSequence(byte b) {
        logError();
        finishSequence();
    }

    private void unknownParameter(int parameter) {
        logError();
    }

    private void logError() {
        finishSequence();
    }

    private void finishSequence() {
        mEscapeState = ESC_NONE;
    }

    private boolean autoWrapEnabled() {
        return (mDecFlags & K_WRAPAROUND_MODE_MASK) != 0;
    }

    private void emit(int c, int style) {
        boolean autoWrap = autoWrapEnabled();
        int width = UnicodeTranscript.charWidth(c);

        if (autoWrap) {
            if (mCursorCol == mColumns - 1 && (mAboutToAutoWrap || width == 2)) {
                mScreen.setLineWrap(mCursorRow);
                mCursorCol = 0;
                mJustWrapped = true;
                if (mCursorRow + 1 < mBottomMargin) {
                    mCursorRow++;
                } else {
                    scroll();
                }
            }
        }

        if (mInsertMode & width != 0) {
            int destCol = mCursorCol + width;
            if (destCol < mColumns) {
                mScreen.blockCopy(mCursorCol, mCursorRow, mColumns - destCol, 1, destCol, mCursorRow);
            }
        }

        if (width == 0) {
            if (mJustWrapped) {
                mScreen.set(mColumns - mLastEmittedCharWidth, mCursorRow - 1, c, style);
            } else {
                mScreen.set(mCursorCol - mLastEmittedCharWidth, mCursorRow, c, style);
            }
        } else {
            mScreen.set(mCursorCol, mCursorRow, c, style);
            mJustWrapped = false;
        }

        if (autoWrap) {
            mAboutToAutoWrap = (mCursorCol == mColumns - 1);
        }

        mCursorCol = Math.min(mCursorCol + width, mColumns - 1);
        if (width > 0) {
            mLastEmittedCharWidth = width;
        }
    }

    private void emit(int c) {
        emit(c, getStyle());
    }

    private void emit(byte b) {
        if (mUseAlternateCharSet && b < 128) {
            emit((int) mSpecialGraphicsCharMap[b]);
        } else {
            emit((int) b);
        }
    }

    private void emit(char[] c) {
        if (Character.isHighSurrogate(c[0])) {
            emit(Character.toCodePoint(c[0], c[1]));
        } else {
            emit((int) c[0]);
        }
    }

    private void emit(char[] c, int offset, int length, int style) {
        for (int i = offset; i < length; ++i) {
            if (c[i] == 0) {
                break;
            }
            if (Character.isHighSurrogate(c[i])) {
                emit(Character.toCodePoint(c[i], c[i + 1]), style);
                ++i;
            } else {
                emit((int) c[i], style);
            }
        }
    }

    private void setCursorRowCol(int row, int col) {
        mCursorRow = Math.min(row, mRows - 1);
        mCursorCol = Math.min(col, mColumns - 1);
        mAboutToAutoWrap = false;
    }

    public int getScrollCounter() {
        return mScrollCounter;
    }

    public void clearScrollCounter() {
        mScrollCounter = 0;
    }

    public void reset() {
        mCursorRow = 0;
        mCursorCol = 0;
        mArgIndex = 0;
        mContinueSequence = false;
        mEscapeState = ESC_NONE;
        mSavedCursorRow = 0;
        mSavedCursorCol = 0;
        mSavedEffect = 0;
        mSavedDecFlags_DECSC_DECRC = 0;
        mDecFlags = 0;
        if (DEFAULT_TO_AUTOWRAP_ENABLED) {
            mDecFlags |= K_WRAPAROUND_MODE_MASK;
        }
        mDecFlags |= K_SHOW_CURSOR_MASK;
        mSavedDecFlags = 0;
        mInsertMode = false;
        mTopMargin = 0;
        mBottomMargin = mRows;
        mAboutToAutoWrap = false;
        mForeColor = mDefaultForeColor;
        mBackColor = mDefaultBackColor;
        mbKeypadApplicationMode = false;
        mAlternateCharSet = false;
        mCharSet[0] = CHAR_SET_ASCII;
        mCharSet[1] = CHAR_SET_SPECIAL_GRAPHICS;
        computeEffectiveCharSet();
        setDefaultTabStops();
        blockClear(0, 0, mColumns, mRows);

        setUTF8Mode(mDefaultUTF8Mode);
        mUTF8EscapeUsed = false;
        mUTF8ToFollow = 0;
        mUTF8ByteBuffer.clear();
        mInputCharBuffer.clear();
    }

    public void setDefaultUTF8Mode(boolean defaultToUTF8Mode) {
        mDefaultUTF8Mode = defaultToUTF8Mode;
        if (!mUTF8EscapeUsed) {
            setUTF8Mode(defaultToUTF8Mode);
        }
    }

    public boolean getUTF8Mode() {
        return mUTF8Mode;
    }

    public void setUTF8Mode(boolean utf8Mode) {
        if (utf8Mode && !mUTF8Mode) {
            mUTF8ToFollow = 0;
            mUTF8ByteBuffer.clear();
            mInputCharBuffer.clear();
        }
        mUTF8Mode = utf8Mode;
        if (mUTF8ModeNotify != null) {
            mUTF8ModeNotify.onUpdate();
        }
    }

    public void setUTF8ModeUpdateCallback(UpdateCallback utf8ModeNotify) {
        mUTF8ModeNotify = utf8ModeNotify;
    }

    public void setColorScheme(ColorScheme scheme) {
        mDefaultForeColor = TextStyle.ciForeground;
        mDefaultBackColor = TextStyle.ciBackground;
    }

    public String getSelectedText(int x1, int y1, int x2, int y2) {
        return mScreen.getSelectedText(x1, y1, x2, y2);
    }
}
