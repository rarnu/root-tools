package com.rarnu.devlib.component;

import android.content.Context;
import android.graphics.Color;
import android.graphics.PixelFormat;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;
import android.view.WindowManager;
import com.rarnu.devlib.intf.FloatWindowListener;

public class FloatWindow extends View implements OnTouchListener {
    long lastDownTime = 0L;
    long lastUpTime = 0L;
    long pressedTime = 0L;
    float downX = 0F, downY = 0F;
    float upX = 0F, upY = 0F;
    @SuppressWarnings("unused")
    private Context context;
    private WindowManager windowMgr;
    private WindowManager.LayoutParams wmParams;
    private View view;
    private float lastX, lastY;
    private FloatWindowListener moveListener;

    public FloatWindow(Context context, View view, FloatWindowListener moveListener) {
        super(context);
        windowMgr = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);
        this.context = context;
        this.moveListener = moveListener;
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
        float x = event.getRawX();
        float y = event.getRawY();

        switch (event.getAction()) {
            case MotionEvent.ACTION_DOWN:
                lastDownTime = System.currentTimeMillis();
                lastX = x;
                lastY = y;
                downX = x;
                downY = y;
                break;
            case MotionEvent.ACTION_MOVE:
                wmParams.x += (int) (x - lastX);
                wmParams.y += (int) (y - lastY);
                windowMgr.updateViewLayout(view, wmParams);
                lastX = x;
                lastY = y;
                if (Math.abs(lastX - downX) < 10 && Math.abs(lastY - downY) < 10) {
                    pressedTime = System.currentTimeMillis();
                    if (lastDownTime != 0) {
                        if ((pressedTime - lastDownTime) > 1500) {
                            lastDownTime = 0L;
                            if (moveListener != null) {
                                moveListener.onFloatWindowLongClick();
                            }
                        }
                    }
                }
                break;
            case MotionEvent.ACTION_UP:
                if (moveListener != null) {
                    moveListener.onPositionChanged(v, wmParams.x, wmParams.y);
                }
                upX = x;
                upY = y;
                if (Math.abs(upX - downX) < 10 && Math.abs(upY - downY) < 10) {
                    lastUpTime = System.currentTimeMillis();
                    if ((lastUpTime - lastDownTime) < 500) {
                        if (moveListener != null) {
                            moveListener.onFloatWindowClick();
                        }
                    }
                }

                break;
        }

        return true;
    }

}
