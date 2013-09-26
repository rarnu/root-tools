package com.rarnu.devlib.component.intf;

import android.view.MotionEvent;

public interface OnScrollOverListener {

    boolean onListViewTopAndPullDown(int delta);

    boolean onListViewBottomAndPullUp(int delta);

    boolean onMotionDown(MotionEvent ev);

    boolean onMotionMove(MotionEvent ev, int delta);

    boolean onMotionUp(MotionEvent ev);

}