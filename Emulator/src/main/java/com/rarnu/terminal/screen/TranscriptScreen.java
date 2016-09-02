package com.rarnu.terminal.screen;

import android.graphics.Canvas;
import com.rarnu.terminal.renderer.StyleRow;
import com.rarnu.terminal.renderer.TextRenderer;
import com.rarnu.terminal.renderer.TextStyle;
import com.rarnu.terminal.utils.ColorScheme;
import com.rarnu.terminal.utils.GrowableIntArray;

import java.util.Arrays;

public class TranscriptScreen implements Screen {

    private int mColumns;
    private int mTotalRows;
    private int mScreenRows;
    private UnicodeTranscript mData;

    public TranscriptScreen(int columns, int totalRows, int screenRows, ColorScheme scheme) {
        init(columns, totalRows, screenRows, TextStyle.kNormalTextStyle);
    }

    private void init(int columns, int totalRows, int screenRows, int style) {
        mColumns = columns;
        mTotalRows = totalRows;
        mScreenRows = screenRows;

        mData = new UnicodeTranscript(columns, totalRows, screenRows, style);
        mData.blockSet(0, 0, mColumns, mScreenRows, ' ', style);
    }

    public void setColorScheme(ColorScheme scheme) {
        mData.setDefaultStyle(TextStyle.kNormalTextStyle);
    }

    public void finish() {
        mData = null;
    }

    public void setLineWrap(int row) {
        mData.setLineWrap(row);
    }

    public void set(int x, int y, int codePoint, int style) {
        mData.setChar(x, y, codePoint, style);
    }

    public void set(int x, int y, byte b, int style) {
        mData.setChar(x, y, b, style);
    }

    public void scroll(int topMargin, int bottomMargin, int style) {
        mData.scroll(topMargin, bottomMargin, style);
    }

    public void blockCopy(int sx, int sy, int w, int h, int dx, int dy) {
        mData.blockCopy(sx, sy, w, h, dx, dy);
    }

    public void blockSet(int sx, int sy, int w, int h, int val, int style) {
        mData.blockSet(sx, sy, w, h, val, style);
    }

    public final void drawText(int row, Canvas canvas, float x, float y, TextRenderer renderer, int cx, int selx1, int selx2, String imeText, int cursorMode) {
        char[] line;
        StyleRow color;
        try {
            line = mData.getLine(row);
            color = mData.getLineColor(row);
        } catch (IllegalArgumentException e) {
            return;
        } catch (NullPointerException e) {
            return;
        }
        int defaultStyle = mData.getDefaultStyle();

        if (line == null) {
            if (selx1 != selx2) {
                char[] blank = new char[selx2 - selx1];
                Arrays.fill(blank, ' ');
                renderer.drawTextRun(canvas, x, y, selx1, selx2 - selx1, blank, 0, 1, true, defaultStyle);
            } else if (cx != -1) {
                renderer.drawCursor(canvas, x, y, cx, cursorMode);
            }

            return;
        }

        int columns = mColumns;
        int lastStyle = 0;
        boolean lastCursorStyle = false;
        int runWidth = 0;
        int lastRunStart = -1;
        int lastRunStartIndex = -1;
        boolean forceFlushRun = false;
        int column = 0;
        int index = 0;
        while (column < columns) {
            int style = color.get(column);
            boolean cursorStyle = false;
            int incr = 1;
            int width;
            if (Character.isHighSurrogate(line[index])) {
                width = UnicodeTranscript.charWidth(line, index);
                incr++;
            } else {
                width = UnicodeTranscript.charWidth(line[index]);
            }
            if (column >= selx1 && column <= selx2) {
                cursorStyle = true;
            }
            if (style != lastStyle || cursorStyle != lastCursorStyle || (width > 0 && forceFlushRun)) {
                if (lastRunStart >= 0) {
                    renderer.drawTextRun(canvas, x, y, lastRunStart, runWidth, line, lastRunStartIndex, index - lastRunStartIndex, lastCursorStyle, lastStyle);
                }
                lastStyle = style;
                lastCursorStyle = cursorStyle;
                runWidth = 0;
                lastRunStart = column;
                lastRunStartIndex = index;
                forceFlushRun = false;
            }
            runWidth += width;
            column += width;
            index += incr;
            if (width > 1) {
                forceFlushRun = true;
            }
        }
        if (lastRunStart >= 0) {
            renderer.drawTextRun(canvas, x, y, lastRunStart, runWidth, line, lastRunStartIndex, index - lastRunStartIndex, lastCursorStyle, lastStyle);
        }

        if (cx >= 0 && imeText.length() > 0) {
            int imeLength = Math.min(columns, imeText.length());
            int imeOffset = imeText.length() - imeLength;
            int imePosition = Math.min(cx, columns - imeLength);
            renderer.drawTextRun(canvas, x, y, imePosition, imeLength, imeText.toCharArray(), imeOffset, imeLength, true, TextStyle.encode(0x0f, 0x00, TextStyle.fxNormal));
        }

        if (cx >= 0) {
            renderer.drawCursor(canvas, x, y, cx, cursorMode);
        }
    }

    public int getActiveRows() {
        return mData.getActiveRows();
    }

    public int getActiveTranscriptRows() {
        return mData.getActiveTranscriptRows();
    }

    public String getTranscriptText() {
        return internalGetTranscriptText(null, 0,
                -mData.getActiveTranscriptRows(), mColumns, mScreenRows);
    }

    public String getTranscriptText(GrowableIntArray colors) {
        return internalGetTranscriptText(colors, 0, -mData.getActiveTranscriptRows(), mColumns, mScreenRows);
    }

    public String getSelectedText(int selX1, int selY1, int selX2, int selY2) {
        return internalGetTranscriptText(null, selX1, selY1, selX2, selY2);
    }

    public String getSelectedText(GrowableIntArray colors, int selX1, int selY1, int selX2, int selY2) {
        return internalGetTranscriptText(colors, selX1, selY1, selX2, selY2);
    }

    private String internalGetTranscriptText(GrowableIntArray colors, int selX1, int selY1, int selX2, int selY2) {
        StringBuilder builder = new StringBuilder();
        UnicodeTranscript data = mData;
        int columns = mColumns;
        char[] line;
        StyleRow rowColorBuffer = null;
        if (selY1 < -data.getActiveTranscriptRows()) {
            selY1 = -data.getActiveTranscriptRows();
        }
        if (selY2 >= mScreenRows) {
            selY2 = mScreenRows - 1;
        }
        for (int row = selY1; row <= selY2; row++) {
            int x1 = 0;
            int x2;
            if (row == selY1) {
                x1 = selX1;
            }
            if (row == selY2) {
                x2 = selX2 + 1;
                if (x2 > columns) {
                    x2 = columns;
                }
            } else {
                x2 = columns;
            }
            line = data.getLine(row, x1, x2);
            if (colors != null) {
                rowColorBuffer = data.getLineColor(row, x1, x2);
            }
            if (line == null) {
                if (!data.getLineWrap(row) && row < selY2 && row < mScreenRows - 1) {
                    builder.append('\n');
                    if (colors != null) {
                        colors.append(0);
                    }
                }
                continue;
            }
            int defaultColor = mData.getDefaultStyle();
            int lastPrintingChar = -1;
            int lineLen = line.length;
            int i;
            int width = x2 - x1;
            int column = 0;
            for (i = 0; i < lineLen && column < width; ++i) {
                char c = line[i];
                if (c == 0) {
                    break;
                } else if (c != ' ' || ((rowColorBuffer != null) && (rowColorBuffer.get(column) != defaultColor))) {
                    lastPrintingChar = i;
                }
                if (!Character.isLowSurrogate(c)) {
                    column += UnicodeTranscript.charWidth(line, i);
                }
            }
            if (data.getLineWrap(row) && lastPrintingChar > -1 && x2 == columns) {
                lastPrintingChar = i - 1;
            }
            builder.append(line, 0, lastPrintingChar + 1);
            if (colors != null) {
                if (rowColorBuffer != null) {
                    column = 0;
                    for (int j = 0; j <= lastPrintingChar; ++j) {
                        colors.append(rowColorBuffer.get(column));
                        column += UnicodeTranscript.charWidth(line, j);
                        if (Character.isHighSurrogate(line[j])) {
                            ++j;
                        }
                    }
                } else {
                    for (int j = 0; j <= lastPrintingChar; ++j) {
                        colors.append(defaultColor);
                        char c = line[j];
                        if (Character.isHighSurrogate(c)) {
                            ++j;
                        }
                    }
                }
            }
            if (!data.getLineWrap(row) && row < selY2 && row < mScreenRows - 1) {
                builder.append('\n');
                if (colors != null) {
                    colors.append((char) 0);
                }
            }
        }
        return builder.toString();
    }

    public boolean fastResize(int columns, int rows, int[] cursor) {
        if (mData == null) {
            return true;
        }
        if (mData.resize(columns, rows, cursor)) {
            mColumns = columns;
            mScreenRows = rows;
            return true;
        } else {
            return false;
        }
    }

    public void resize(int columns, int rows, int style) {
        init(columns, mTotalRows, rows, style);
    }
}
