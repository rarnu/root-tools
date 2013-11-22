package com.rarnu.devlib.component;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.RectF;
import android.util.AttributeSet;
import android.view.View;

public class DotView extends View {

    public interface CountdownListener {
        void onCountdown(View v);
    }

    public DotView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        init();
    }

    public DotView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public DotView(Context context) {
        super(context);
        init();
    }

    private Paint mPaints;
    private Paint mPaintBackground;
    private RectF mOvals = null;
    private float mSweep = 0F;
    private boolean start = false;
    private CountdownListener listener;

    private float mStartSweep = 0F;
    private float mSweepInc = 1F;
    private int mPaintColor = Color.WHITE;
    private int mBackgroundFillColor = Color.BLACK;
    private int mSizeWidth = 0, mSizeHeight = 0;

    public void setBackgroundSize(int width, int height) {
        mSizeWidth = width;
        mSizeHeight = height;
    }

    public void setPaintColor(int color) {
        this.mPaintColor = color;
        mPaints.setColor(mPaintColor);
    }

    public int getPaintColor() {
        return mPaintColor;
    }

    public void setBackgroundFillColor(int color) {
        this.mBackgroundFillColor = color;
        mPaintBackground.setColor(mBackgroundFillColor);
    }


    public int getBackgroundFillColor() {
        return mBackgroundFillColor;
    }


    public void setStartSweep(float startSweep) {
        this.mStartSweep = startSweep;
    }

    public float getStartSweep() {
        return mStartSweep;
    }

    public void setSweepInc(float sweepInc) {
        this.mSweepInc = sweepInc;
    }

    public float getSweepInc() {
        return mSweepInc;
    }

    private void init() {
        mPaints = new Paint();
        mPaints.setAntiAlias(true);
        mPaints.setStyle(Paint.Style.FILL);
        mPaints.setColor(mPaintColor);

        mPaintBackground = new Paint(mPaints);
        mPaintBackground.setColor(mBackgroundFillColor);

    }

    private void drawArcs(Canvas canvas) {
        canvas.drawArc(mOvals, -90F, mSweep, true, mPaints);
    }

    private void drawBackground(Canvas canvas) {
        canvas.drawArc(mOvals, 0F, 360F, true, mPaintBackground);
    }

    public void reset() {
        mSweep = mStartSweep;
        invalidate();
    }

    public void start(boolean reset) {
        start = true;
        if (reset) {
            mSweep = mStartSweep;
        }
        invalidate();
    }

    public void stop(boolean reset) {
        start = false;
        if (reset) {
            mSweep = mStartSweep;
            invalidate();
        }
    }

    @Override
    protected void onDraw(Canvas canvas) {
        if (mOvals == null) {
            mOvals = new RectF(0, 0, mSizeWidth, mSizeHeight);
        }
        drawBackground(canvas);
        drawArcs(canvas);

        if (start) {
            mSweep += mSweepInc;
            if (mSweep > 360) {
                mSweep -= 360;
                if (listener != null) {
                    listener.onCountdown(this);
                }

            }
            invalidate();
        }
    }

    public CountdownListener getCountdownListener() {
        return listener;
    }

    public void setCountdownListener(CountdownListener listener) {
        this.listener = listener;
    }
}