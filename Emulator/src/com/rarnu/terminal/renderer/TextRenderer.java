package com.rarnu.terminal.renderer;

import android.graphics.Canvas;

public interface TextRenderer {
    public static final int MODE_OFF = 0;
    public static final int MODE_ON = 1;
    public static final int MODE_LOCKED = 2;
    public static final int MODE_MASK = 3;
    public static final int MODE_SHIFT_SHIFT = 0;
    public static final int MODE_ALT_SHIFT = 2;
    public static final int MODE_CTRL_SHIFT = 4;
    public static final int MODE_FN_SHIFT = 6;

    void setReverseVideo(boolean reverseVideo);

    float getCharacterWidth();

    int getCharacterHeight();

    int getTopMargin();

    void drawTextRun(Canvas canvas, float x, float y, int lineOffset, int runWidth, char[] text, int index, int count, boolean cursor, int textStyle);

    void drawCursor(Canvas canvas, float x, float y, int lineOffset, int cursorMode);
}
