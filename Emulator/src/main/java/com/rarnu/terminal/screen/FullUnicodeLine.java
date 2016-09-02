package com.rarnu.terminal.screen;

class FullUnicodeLine {
    private static final float SPARE_CAPACITY_FACTOR = 1.5f;
    private char[] mText;
    private short[] mOffset;
    private int mColumns;

    public FullUnicodeLine(int columns) {
        commonConstructor(columns);
        char[] text = mText;
        for (int i = 0; i < columns; ++i) {
            text[i] = ' ';
        }
        mOffset[0] = (short) columns;
    }

    public FullUnicodeLine(char[] basicLine) {
        commonConstructor(basicLine.length);
        System.arraycopy(basicLine, 0, mText, 0, mColumns);
        mOffset[0] = (short) basicLine.length;
    }

    private void commonConstructor(int columns) {
        mColumns = columns;
        mOffset = new short[columns];
        mText = new char[(int) (SPARE_CAPACITY_FACTOR * columns)];
    }

    public int getSpaceUsed() {
        return mOffset[0];
    }

    public char[] getLine() {
        return mText;
    }

    public int findStartOfColumn(int column) {
        if (column == 0) {
            return 0;
        } else {
            return column + mOffset[column];
        }
    }

    public boolean getChar(int column, int charIndex, char[] out, int offset) {
        int pos = findStartOfColumn(column);
        int length;
        if (column + 1 < mColumns) {
            length = findStartOfColumn(column + 1) - pos;
        } else {
            length = getSpaceUsed() - pos;
        }
        if (charIndex >= length) {
            throw new IllegalArgumentException();
        }
        out[offset] = mText[pos + charIndex];
        return (charIndex + 1 < length);
    }

    public void setChar(int column, int codePoint) {
        int columns = mColumns;
        if (column < 0 || column >= columns) {
            throw new IllegalArgumentException();
        }

        char[] text = mText;
        short[] offset = mOffset;
        int spaceUsed = offset[0];

        int pos = findStartOfColumn(column);

        int charWidth = UnicodeTranscript.charWidth(codePoint);
        int oldCharWidth = UnicodeTranscript.charWidth(text, pos);

        int oldLen;
        if (column + oldCharWidth < columns) {
            oldLen = findStartOfColumn(column + oldCharWidth) - pos;
        } else {
            oldLen = spaceUsed - pos;
        }

        int newLen = Character.charCount(codePoint);
        if (charWidth == 0) {
            newLen += oldLen;
        }
        int shift = newLen - oldLen;

        if (shift > 0) {
            if (spaceUsed + shift > text.length) {
                char[] newText = new char[text.length + columns];
                System.arraycopy(text, 0, newText, 0, pos);
                System.arraycopy(text, pos + oldLen, newText, pos + newLen, spaceUsed - pos - oldLen);
                mText = text = newText;
            } else {
                System.arraycopy(text, pos + oldLen, text, pos + newLen, spaceUsed - pos - oldLen);
            }
        }

        if (charWidth > 0) {
            Character.toChars(codePoint, text, pos);
        } else {
            Character.toChars(codePoint, text, pos + oldLen);
        }

        if (shift < 0) {
            System.arraycopy(text, pos + oldLen, text, pos + newLen, spaceUsed - pos - oldLen);
        }

        if (shift != 0) {
            spaceUsed += shift;
            offset[0] = (short) spaceUsed;
        }

        if (oldCharWidth == 2 && charWidth == 1) {

            int nextPos = pos + newLen;
            if (spaceUsed + 1 > text.length) {
                char[] newText = new char[text.length + columns];
                System.arraycopy(text, 0, newText, 0, nextPos);

                System.arraycopy(text, nextPos, newText, nextPos + 1, spaceUsed - nextPos);
                mText = text = newText;
            } else {
                System.arraycopy(text, nextPos, text, nextPos + 1, spaceUsed - nextPos);
            }
            text[nextPos] = ' ';

            ++offset[0];
            if (column == 0) {
                offset[1] = (short) (newLen - 1);
            } else if (column + 1 < columns) {
                offset[column + 1] = (short) (offset[column] + newLen - 1);
            }
            ++column;
            ++shift;
        } else if (oldCharWidth == 1 && charWidth == 2) {
            if (column == columns - 1) {
                text[pos] = ' ';
                offset[0] = (short) (pos + 1);
                shift = 0;
            } else if (column == columns - 2) {
                offset[column + 1] = (short) (offset[column] - 1);
                offset[0] = (short) (pos + newLen);
                shift = 0;
            } else {
                int nextPos = pos + newLen;
                int nextWidth = UnicodeTranscript.charWidth(text, nextPos);
                int nextLen;
                if (column + nextWidth + 1 < columns) {
                    nextLen = findStartOfColumn(column + nextWidth + 1) + shift - nextPos;
                } else {
                    nextLen = spaceUsed - nextPos;
                }

                if (nextWidth == 2) {
                    text[nextPos] = ' ';
                    if (nextLen > 1) {
                        System.arraycopy(text, nextPos + nextLen, text, nextPos + 1, spaceUsed - nextPos - nextLen);
                        shift -= nextLen - 1;
                        offset[0] -= nextLen - 1;
                    }
                } else {
                    System.arraycopy(text, nextPos + nextLen, text, nextPos, spaceUsed - nextPos - nextLen);
                    shift -= nextLen;

                    offset[0] -= nextLen;
                }

                if (column == 0) {
                    offset[1] = -1;
                } else {
                    offset[column + 1] = (short) (offset[column] - 1);
                }
                ++column;
            }
        }

        if (shift != 0) {
            for (int i = column + 1; i < columns; ++i) {
                offset[i] += shift;
            }
        }
    }
}