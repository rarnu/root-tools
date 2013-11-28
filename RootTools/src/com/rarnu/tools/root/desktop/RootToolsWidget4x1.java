package com.rarnu.tools.root.desktop;

import android.content.Context;
import android.content.Intent;
import android.widget.RemoteViews;
import com.rarnu.devlib.base.BaseAppWidget;
import com.rarnu.tools.root.R;

public class RootToolsWidget4x1 extends BaseAppWidget {
    @Override
    public void onReceive(Context context, Intent intent) {
        super.onReceive(context, intent);
    }

    @Override
    protected int getWidgetLayoutResId() {
        return R.layout.widget_desktop_4x1;
    }

    @Override
    protected void initComponents(RemoteViews views) {

    }

    @Override
    protected void initEvents(RemoteViews views) {

    }

    @Override
    protected void initLogic(RemoteViews views) {

    }


}
