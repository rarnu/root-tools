package com.rarnu.terminal.screen;

import com.rarnu.terminal.utils.GrowableIntArray;

public interface Screen {

    void setLineWrap(int row);

    void set(int x, int y, int codePoint, int style);

    void set(int x, int y, byte b, int style);

    void scroll(int topMargin, int bottomMargin, int style);

    void blockCopy(int sx, int sy, int w, int h, int dx, int dy);

    void blockSet(int sx, int sy, int w, int h, int val, int style);

    String getTranscriptText();

    String getTranscriptText(GrowableIntArray colors);

    String getSelectedText(int x1, int y1, int x2, int y2);

    String getSelectedText(GrowableIntArray colors, int x1, int y1, int x2, int y2);

    int getActiveRows();

    boolean fastResize(int columns, int rows, int[] cursor);

    void resize(int columns, int rows, int style);
}
