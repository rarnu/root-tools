package com.rarnu.devlib.intf;

import android.view.View;

public interface FloatWindowListener {
    void onPositionChanged(View v, int x, int y);
    void onFloatWindowClick();
    void onFloatWindowLongClick();
}
