package com.rarnu.devlib.component;

import android.content.Context;
import android.graphics.Color;
import android.graphics.PixelFormat;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;
import android.view.WindowManager;

public class FloatWindow extends View implements OnTouchListener {
    @SuppressWarnings("unused")
    private Context context;
    private WindowManager windowMgr;
    private WindowManager.LayoutParams wmParams;
    private View view;
    private float lastX, lastY;

    public FloatWindow(Context context, View view) {
        super(context);
        windowMgr = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);
        this.context = context;
        initView(view);
    }

    private void initView(View view) {
        this.view = view;
        this.view.setBackgroundColor(Color.TRANSPARENT);
        this.view.setOnTouchListener(this);
    }

    public void show(int x, int y) {

        wmParams = new WindowManager.LayoutParams();
        wmParams.type = WindowManager.LayoutParams.TYPE_SYSTEM_ALERT | WindowManager.LayoutParams.TYPE_SYSTEM_OVERLAY;
        wmParams.flags = WindowManager.LayoutParams.FLAG_NOT_TOUCH_MODAL | WindowManager.LayoutParams.FLAG_NOT_FOCUSABLE;
        wmParams.width = WindowManager.LayoutParams.WRAP_CONTENT;
        wmParams.height = WindowManager.LayoutParams.WRAP_CONTENT;
        wmParams.format = PixelFormat.TRANSLUCENT;

        if (x != -1) {
            wmParams.x = x;
        }
        if (y != -1) {
            wmParams.y = y;
        }

        windowMgr.addView(view, wmParams);
    }

    public void hide() {
        windowMgr.removeView(view);
    }

    public void setNewView(View view, int x, int y) {
        hide();
        initView(view);
        show(x, y);
    }

    @Override
    public boolean onTouch(View v, MotionEvent event) {
        float x = event.getX();
        float y = event.getY();

        switch (event.getAction()) {
            case MotionEvent.ACTION_DOWN:
                lastX = x;
                lastY = y;
                break;
            case MotionEvent.ACTION_MOVE:
                wmParams.x += (int) (x - lastX);
                wmParams.y += (int) (y - lastY);
                windowMgr.updateViewLayout(view, wmParams);
                lastX = x;
                lastY = y;
                break;
        }

        return true;
    }

}
