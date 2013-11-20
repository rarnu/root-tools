package com.rarnu.devlib.component.intf;

import android.view.View;

public interface FloatWindowListener {
    void onPositionChanged(View v, float x, float y);
    void onFloatWindowClick();
    void onFloatWindowLongClick();
}
