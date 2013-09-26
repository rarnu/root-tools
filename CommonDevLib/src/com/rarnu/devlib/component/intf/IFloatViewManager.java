package com.rarnu.devlib.component.intf;

import android.graphics.Point;
import android.view.View;

public interface IFloatViewManager {

    public View onCreateFloatView(int position);

    public void onDragFloatView(View floatView, Point location, Point touch);

    public void onDestroyFloatView(View floatView);
}