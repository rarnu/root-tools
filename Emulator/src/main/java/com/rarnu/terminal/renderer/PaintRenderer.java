package com.rarnu.terminal.renderer;

import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Typeface;
import com.rarnu.terminal.utils.ColorScheme;

public class PaintRenderer extends BaseTextRenderer {

    private static final char[] EXAMPLE_CHAR = {'X'};
    private Paint mTextPaint;
    private float mCharWidth;
    private int mCharHeight;
    private int mCharAscent;
    private int mCharDescent;

    public PaintRenderer(int fontSize, ColorScheme scheme) {
        super(scheme);
        mTextPaint = new Paint();
        mTextPaint.setTypeface(Typeface.MONOSPACE);
        mTextPaint.setAntiAlias(true);
        mTextPaint.setTextSize(fontSize);

        mCharHeight = (int) Math.ceil(mTextPaint.getFontSpacing());
        mCharAscent = (int) Math.ceil(mTextPaint.ascent());
        mCharDescent = mCharHeight + mCharAscent;
        mCharWidth = mTextPaint.measureText(EXAMPLE_CHAR, 0, 1);
    }

    public void drawTextRun(Canvas canvas, float x, float y, int lineOffset, int runWidth, char[] text, int index, int count, boolean cursor, int textStyle) {
        int foreColor = TextStyle.decodeForeColor(textStyle);
        int backColor = TextStyle.decodeBackColor(textStyle);
        int effect = TextStyle.decodeEffect(textStyle);

        boolean inverse = mReverseVideo ^ (effect & (TextStyle.fxInverse | TextStyle.fxItalic)) != 0;
        if (inverse) {
            int temp = foreColor;
            foreColor = backColor;
            backColor = temp;
        }

        if (cursor) {
            backColor = TextStyle.ciCursor;
        }

        mTextPaint.setColor(mPalette[backColor]);

        float left = x + lineOffset * mCharWidth;
        canvas.drawRect(left, y + mCharAscent - mCharDescent, left + runWidth * mCharWidth, y, mTextPaint);
        boolean invisible = (effect & TextStyle.fxInvisible) != 0;
        if (!invisible) {
            boolean bold = (effect & (TextStyle.fxBold | TextStyle.fxBlink)) != 0;
            boolean underline = (effect & TextStyle.fxUnderline) != 0;
            if (bold) {
                mTextPaint.setFakeBoldText(true);
            }
            if (underline) {
                mTextPaint.setUnderlineText(true);
            }
            if (foreColor < 8 && bold) {
                mTextPaint.setColor(mPalette[foreColor + 8]);
            } else {
                mTextPaint.setColor(mPalette[foreColor]);
            }
            canvas.drawText(text, index, count, left, y - mCharDescent, mTextPaint);
            if (bold) {
                mTextPaint.setFakeBoldText(false);
            }
            if (underline) {
                mTextPaint.setUnderlineText(false);
            }
        }
    }

    public void drawCursor(Canvas canvas, float x, float y, int lineOffset, int cursorMode) {
        float left = x + lineOffset * mCharWidth;
        drawCursorImp(canvas, left, y, mCharWidth, mCharHeight, cursorMode);
    }

    public int getCharacterHeight() {
        return mCharHeight;
    }

    public float getCharacterWidth() {
        return mCharWidth;
    }

    public int getTopMargin() {
        return mCharDescent;
    }
}
