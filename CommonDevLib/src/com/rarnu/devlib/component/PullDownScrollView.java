package com.rarnu.devlib.component;

import android.content.Context;
import android.os.Handler;
import android.util.AttributeSet;
import android.view.View;
import android.widget.ScrollView;

public class PullDownScrollView extends ScrollView {

    public PullDownScrollView(Context context) {
        super(context);
        init(context);

    }

    public PullDownScrollView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init(context);

    }

    public PullDownScrollView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        init(context);
    }

    public static void ScrollToPoint(final View scroll, final View inner, final int i) {
        Handler mHandler = new Handler();

        mHandler.post(new Runnable() {
            public void run() {
                if (scroll == null || inner == null) {
                    return;
                }

                int offset = inner.getMeasuredHeight() - scroll.getHeight() - i;

                if (offset < 0) {
                    offset = 0;
                }

                scroll.scrollTo(0, offset);
                scroll.invalidate();
            }
        });
    }

    private void init(Context context) {

    }

}