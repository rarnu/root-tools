package com.rarnu.terminal.screen;

import android.text.AndroidCharacter;
import com.rarnu.terminal.renderer.StyleRow;

public class UnicodeTranscript {

    private Object[] mLines;
    private StyleRow[] mColor;
    private boolean[] mLineWrap;
    private int mTotalRows;
    private int mScreenRows;
    private int mColumns;
    private int mActiveTranscriptRows = 0;
    private int mDefaultStyle = 0;
    private int mScreenFirstRow = 0;
    private char[] tmpLine;
    private StyleRow tmpColor;

    public UnicodeTranscript(int columns, int totalRows, int screenRows, int defaultStyle) {
        mColumns = columns;
        mTotalRows = totalRows;
        mScreenRows = screenRows;
        mLines = new Object[totalRows];
        mColor = new StyleRow[totalRows];
        mLineWrap = new boolean[totalRows];
        tmpColor = new StyleRow(defaultStyle, mColumns);

        mDefaultStyle = defaultStyle;
    }

    public static int charWidth(int codePoint) {

        if (codePoint > 31 && codePoint < 127) {
            return 1;
        }

        if (codePoint == 27) {
            return 1;
        }

        switch (Character.getType(codePoint)) {
            case Character.CONTROL:
            case Character.FORMAT:
            case Character.NON_SPACING_MARK:
            case Character.ENCLOSING_MARK:
                return 0;
        }

        if (Character.charCount(codePoint) == 1) {
            switch (AndroidCharacter.getEastAsianWidth((char) codePoint)) {
                case AndroidCharacter.EAST_ASIAN_WIDTH_FULL_WIDTH:
                case AndroidCharacter.EAST_ASIAN_WIDTH_WIDE:
                    return 2;
            }
        } else {
            switch ((codePoint >> 16) & 0xf) {
                case 2:
                case 3:
                    return 2;
            }
        }

        return 1;
    }

    public static int charWidth(char cHigh, char cLow) {
        return charWidth(Character.toCodePoint(cHigh, cLow));
    }

    public static int charWidth(char[] chars, int index) {
        char c = chars[index];
        if (Character.isHighSurrogate(c)) {
            return charWidth(c, chars[index + 1]);
        } else {
            return charWidth(c);
        }
    }

    public int getDefaultStyle() {
        return mDefaultStyle;
    }

    public void setDefaultStyle(int defaultStyle) {
        mDefaultStyle = defaultStyle;
    }

    public int getActiveTranscriptRows() {
        return mActiveTranscriptRows;
    }

    public int getActiveRows() {
        return mActiveTranscriptRows + mScreenRows;
    }

    private int externalToInternalRow(int extRow) {
        if (extRow < -mActiveTranscriptRows || extRow > mScreenRows) {
            String errorMessage = "externalToInternalRow " + extRow + " " + mScreenRows + " " + mActiveTranscriptRows;
            throw new IllegalArgumentException(errorMessage);
        }

        if (extRow >= 0) {
            return (mScreenFirstRow + extRow) % mTotalRows;
        } else {
            if (-extRow > mScreenFirstRow) {
                return mTotalRows + mScreenFirstRow + extRow;
            } else {
                return mScreenFirstRow + extRow;
            }
        }
    }

    public void setLineWrap(int row) {
        mLineWrap[externalToInternalRow(row)] = true;
    }

    public boolean getLineWrap(int row) {
        return mLineWrap[externalToInternalRow(row)];
    }

    public boolean resize(int newColumns, int newRows, int[] cursor) {
        if (newColumns != mColumns || newRows > mTotalRows) {
            return false;
        }

        int screenRows = mScreenRows;
        int activeTranscriptRows = mActiveTranscriptRows;
        int shift = screenRows - newRows;
        if (shift < -activeTranscriptRows) {
            Object[] lines = mLines;
            Object[] color = mColor;
            boolean[] lineWrap = mLineWrap;
            int screenFirstRow = mScreenFirstRow;
            int totalRows = mTotalRows;
            for (int i = 0; i < activeTranscriptRows - shift; ++i) {
                int index = (screenFirstRow + screenRows + i) % totalRows;
                lines[index] = null;
                color[index] = null;
                lineWrap[index] = false;
            }
            shift = -activeTranscriptRows;
        } else if (shift > 0 && cursor[1] != screenRows - 1) {
            Object[] lines = mLines;
            for (int i = screenRows - 1; i > cursor[1]; --i) {
                int index = externalToInternalRow(i);
                if (lines[index] == null) {
                    --shift;
                    if (shift == 0) {
                        break;
                    } else {
                        continue;
                    }
                }

                char[] line;
                if (lines[index] instanceof char[]) {
                    line = (char[]) lines[index];
                } else {
                    line = ((FullUnicodeLine) lines[index]).getLine();
                }

                int len = line.length;
                int j;
                for (j = 0; j < len; ++j) {
                    if (line[j] == 0) {
                        j = len;
                        break;
                    } else if (line[j] != ' ') {
                        break;
                    }
                }

                if (j == len) {
                    --shift;
                    if (shift == 0) {
                        break;
                    } else {
                        continue;
                    }
                } else {
                    break;
                }
            }
        }

        if (shift > 0 || (shift < 0 && mScreenFirstRow >= -shift)) {
            mScreenFirstRow = (mScreenFirstRow + shift) % mTotalRows;
        } else if (shift < 0) {
            mScreenFirstRow = mTotalRows + mScreenFirstRow + shift;
        }

        if (mActiveTranscriptRows + shift < 0) {
            mActiveTranscriptRows = 0;
        } else {
            mActiveTranscriptRows += shift;
        }
        cursor[1] -= shift;
        mScreenRows = newRows;

        return true;
    }

    private void blockCopyLines(int src, int len, int shift) {
        int totalRows = mTotalRows;

        int dst;
        if (src + shift >= 0) {
            dst = (src + shift) % totalRows;
        } else {
            dst = totalRows + src + shift;
        }

        if (src + len <= totalRows && dst + len <= totalRows) {
            System.arraycopy(mLines, src, mLines, dst, len);
            System.arraycopy(mColor, src, mColor, dst, len);
            System.arraycopy(mLineWrap, src, mLineWrap, dst, len);
            return;
        }

        if (shift < 0) {
            for (int i = 0; i < len; ++i) {
                mLines[(dst + i) % totalRows] = mLines[(src + i) % totalRows];
                mColor[(dst + i) % totalRows] = mColor[(src + i) % totalRows];
                mLineWrap[(dst + i) % totalRows] = mLineWrap[(src + i) % totalRows];
            }
        } else {
            for (int i = len - 1; i >= 0; --i) {
                mLines[(dst + i) % totalRows] = mLines[(src + i) % totalRows];
                mColor[(dst + i) % totalRows] = mColor[(src + i) % totalRows];
                mLineWrap[(dst + i) % totalRows] = mLineWrap[(src + i) % totalRows];
            }
        }
    }

    public void scroll(int topMargin, int bottomMargin, int style) {
        if (topMargin > bottomMargin - 1) {
            throw new IllegalArgumentException();
        }

        if (topMargin < 0) {
            throw new IllegalArgumentException();
        }

        if (bottomMargin > mScreenRows) {
            throw new IllegalArgumentException();
        }

        int screenRows = mScreenRows;
        int totalRows = mTotalRows;

        if (topMargin == 0 && bottomMargin == screenRows) {
            mScreenFirstRow = (mScreenFirstRow + 1) % totalRows;
            if (mActiveTranscriptRows < totalRows - screenRows) {
                ++mActiveTranscriptRows;
            }
            int blankRow = externalToInternalRow(bottomMargin - 1);
            mLines[blankRow] = null;
            mColor[blankRow] = new StyleRow(style, mColumns);
            mLineWrap[blankRow] = false;

            return;
        }

        int screenFirstRow = mScreenFirstRow;
        int topMarginInt = externalToInternalRow(topMargin);
        int bottomMarginInt = externalToInternalRow(bottomMargin);

        Object[] lines = mLines;
        StyleRow[] color = mColor;
        boolean[] lineWrap = mLineWrap;
        Object scrollLine = lines[topMarginInt];
        StyleRow scrollColor = color[topMarginInt];
        boolean scrollLineWrap = lineWrap[topMarginInt];
        blockCopyLines(screenFirstRow, topMargin, 1);
        blockCopyLines(bottomMarginInt, screenRows - bottomMargin, 1);
        lines[screenFirstRow] = scrollLine;
        color[screenFirstRow] = scrollColor;
        lineWrap[screenFirstRow] = scrollLineWrap;

        mScreenFirstRow = (screenFirstRow + 1) % totalRows;
        if (mActiveTranscriptRows < totalRows - screenRows) {
            ++mActiveTranscriptRows;
        }

        int blankRow = externalToInternalRow(bottomMargin - 1);
        lines[blankRow] = null;
        color[blankRow] = new StyleRow(style, mColumns);
        lineWrap[blankRow] = false;

        return;
    }

    public void blockCopy(int sx, int sy, int w, int h, int dx, int dy) {
        if (sx < 0 || sx + w > mColumns || sy < 0 || sy + h > mScreenRows || dx < 0 || dx + w > mColumns || dy < 0 || dy + h > mScreenRows) {
            throw new IllegalArgumentException();
        }
        Object[] lines = mLines;
        StyleRow[] color = mColor;
        if (sy > dy) {
            for (int y = 0; y < h; y++) {
                int srcRow = externalToInternalRow(sy + y);
                int dstRow = externalToInternalRow(dy + y);
                if (lines[srcRow] instanceof char[] && lines[dstRow] instanceof char[]) {
                    System.arraycopy(lines[srcRow], sx, lines[dstRow], dx, w);
                } else {
                    int extDstRow = dy + y;
                    char[] tmp = getLine(sy + y, sx, sx + w);
                    if (tmp == null) {
                        blockSet(dx, extDstRow, w, 1, ' ', mDefaultStyle);
                        continue;
                    }
                    char cHigh = 0;
                    int x = 0;
                    int columns = mColumns;
                    for (int i = 0; i < tmp.length; ++i) {
                        if (tmp[i] == 0 || dx + x >= columns) {
                            break;
                        }
                        if (Character.isHighSurrogate(tmp[i])) {
                            cHigh = tmp[i];
                            continue;
                        } else if (Character.isLowSurrogate(tmp[i])) {
                            int codePoint = Character.toCodePoint(cHigh, tmp[i]);
                            setChar(dx + x, extDstRow, codePoint);
                            x += charWidth(codePoint);
                        } else {
                            setChar(dx + x, extDstRow, tmp[i]);
                            x += charWidth(tmp[i]);
                        }
                    }
                }
                color[srcRow].copy(sx, color[dstRow], dx, w);
            }
        } else {
            for (int y = 0; y < h; y++) {
                int y2 = h - (y + 1);
                int srcRow = externalToInternalRow(sy + y2);
                int dstRow = externalToInternalRow(dy + y2);
                if (lines[srcRow] instanceof char[] && lines[dstRow] instanceof char[]) {
                    System.arraycopy(lines[srcRow], sx, lines[dstRow], dx, w);
                } else {
                    int extDstRow = dy + y2;
                    char[] tmp = getLine(sy + y2, sx, sx + w);
                    if (tmp == null) {
                        blockSet(dx, extDstRow, w, 1, ' ', mDefaultStyle);
                        continue;
                    }
                    char cHigh = 0;
                    int x = 0;
                    int columns = mColumns;
                    for (int i = 0; i < tmp.length; ++i) {
                        if (tmp[i] == 0 || dx + x >= columns) {
                            break;
                        }
                        if (Character.isHighSurrogate(tmp[i])) {
                            cHigh = tmp[i];
                            continue;
                        } else if (Character.isLowSurrogate(tmp[i])) {
                            int codePoint = Character.toCodePoint(cHigh, tmp[i]);
                            setChar(dx + x, extDstRow, codePoint);
                            x += charWidth(codePoint);
                        } else {
                            setChar(dx + x, extDstRow, tmp[i]);
                            x += charWidth(tmp[i]);
                        }
                    }
                }
                color[srcRow].copy(sx, color[dstRow], dx, w);
            }
        }
    }

    public void blockSet(int sx, int sy, int w, int h, int val, int style) {
        if (sx < 0 || sx + w > mColumns || sy < 0 || sy + h > mScreenRows) {
            throw new IllegalArgumentException();
        }

        for (int y = 0; y < h; y++) {
            for (int x = 0; x < w; x++) {
                setChar(sx + x, sy + y, val, style);
            }
        }
    }

    public char[] getLine(int row, int x1, int x2) {
        if (row < -mActiveTranscriptRows || row > mScreenRows - 1) {
            throw new IllegalArgumentException();
        }

        int columns = mColumns;
        row = externalToInternalRow(row);
        if (mLines[row] == null) {
            return null;
        }
        if (mLines[row] instanceof char[]) {
            if (x1 == 0 && x2 == columns) {
                return (char[]) mLines[row];
            } else {
                if (tmpLine == null || tmpLine.length < columns + 1) {
                    tmpLine = new char[columns + 1];
                }
                int length = x2 - x1;
                System.arraycopy(mLines[row], x1, tmpLine, 0, length);
                tmpLine[length] = 0;
                return tmpLine;
            }
        }

        FullUnicodeLine line = (FullUnicodeLine) mLines[row];
        char[] rawLine = line.getLine();
        x1 = line.findStartOfColumn(x1);
        if (x2 < columns) {
            x2 = line.findStartOfColumn(x2);
        } else {
            x2 = line.getSpaceUsed();
        }
        int length = x2 - x1;

        if (tmpLine == null || tmpLine.length < length + 1) {
            tmpLine = new char[length + 1];
        }
        System.arraycopy(rawLine, x1, tmpLine, 0, length);
        tmpLine[length] = 0;
        return tmpLine;
    }

    public char[] getLine(int row) {
        return getLine(row, 0, mColumns);
    }

    public StyleRow getLineColor(int row, int x1, int x2) {
        if (row < -mActiveTranscriptRows || row > mScreenRows - 1) {
            throw new IllegalArgumentException();
        }

        row = externalToInternalRow(row);
        StyleRow color = mColor[row];
        StyleRow tmp = tmpColor;
        if (color != null) {
            if (x1 == 0 && x2 == mColumns) {
                return color;
            }
            color.copy(x1, tmp, 0, x2 - x1);
            return tmp;
        } else {
            return null;
        }
    }

    public StyleRow getLineColor(int row) {
        return getLineColor(row, 0, mColumns);
    }

    public boolean getChar(int row, int column) {
        return getChar(row, column, 0);
    }

    public boolean getChar(int row, int column, int charIndex) {
        return getChar(row, column, charIndex, new char[1], 0);
    }

    public boolean getChar(int row, int column, int charIndex, char[] out, int offset) {
        if (row < -mActiveTranscriptRows || row > mScreenRows - 1) {
            throw new IllegalArgumentException();
        }
        row = externalToInternalRow(row);

        if (mLines[row] instanceof char[]) {
            char[] line = (char[]) mLines[row];
            out[offset] = line[column];
            return false;
        }

        FullUnicodeLine line = (FullUnicodeLine) mLines[row];
        return line.getChar(column, charIndex, out, offset);
    }

    private boolean isBasicChar(int codePoint) {
        return !(charWidth(codePoint) != 1 || Character.charCount(codePoint) != 1);
    }

    private char[] allocateBasicLine(int row, int columns) {
        char[] line = new char[columns];
        for (int i = 0; i < columns; ++i) {
            line[i] = ' ';
        }

        mLines[row] = line;
        if (mColor[row] == null) {
            mColor[row] = new StyleRow(0, columns);
        }
        return line;
    }

    private FullUnicodeLine allocateFullLine(int row, int columns) {
        FullUnicodeLine line = new FullUnicodeLine(columns);

        mLines[row] = line;
        if (mColor[row] == null) {
            mColor[row] = new StyleRow(0, columns);
        }
        return line;
    }

    public boolean setChar(int column, int row, int codePoint, int style) {
        if (!setChar(column, row, codePoint)) {
            return false;
        }

        row = externalToInternalRow(row);
        mColor[row].set(column, style);

        return true;
    }

    public boolean setChar(int column, int row, int codePoint) {
        if (row >= mScreenRows || column >= mColumns) {
            throw new IllegalArgumentException();
        }
        row = externalToInternalRow(row);

        int basicMode = -1;

        if (mLines[row] == null) {
            if (isBasicChar(codePoint)) {
                allocateBasicLine(row, mColumns);
                basicMode = 1;
            } else {
                allocateFullLine(row, mColumns);
                basicMode = 0;
            }
        }

        if (mLines[row] instanceof char[]) {
            char[] line = (char[]) mLines[row];

            if (basicMode == -1) {
                if (isBasicChar(codePoint)) {
                    basicMode = 1;
                } else {
                    basicMode = 0;
                }
            }

            if (basicMode == 1) {
                line[column] = (char) codePoint;
                return true;
            }

            mLines[row] = new FullUnicodeLine(line);
        }

        FullUnicodeLine line = (FullUnicodeLine) mLines[row];
        line.setChar(column, codePoint);
        return true;
    }
}
