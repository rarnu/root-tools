package com.rarnu.tools.root.desktop;

import android.content.Context;
import android.content.Intent;
import android.graphics.Color;
import android.view.View;
import android.widget.RemoteViews;
import android.widget.Toast;
import com.rarnu.devlib.base.BaseAppWidget;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.Actions;
import com.rarnu.tools.root.common.MemoryInfo;
import com.rarnu.tools.root.common.RTConfig;
import com.rarnu.tools.root.service.AutoCleanMemoryService;
import com.rarnu.tools.root.service.WidgetKeepService;
import com.rarnu.tools.root.utils.ApkUtils;
import com.rarnu.tools.root.utils.MemoryUtils;

public class RootToolsWidget4x1 extends BaseAppWidget {

    @Override
    protected int getWidgetLayoutResId() {
        return R.layout.widget_desktop_4x1;
    }

    @Override
    protected void initComponents(Context context, RemoteViews views) {

    }

    @Override
    protected void initEvents(Context context, RemoteViews views) {
        registerEvent(context, Actions.ACTION_WIDGET_CLEAN, R.id.wbtn_clean);
        registerEvent(context, Actions.ACTION_WIDGET_LAUNCH, R.id.wbtn_launch);
    }

    @Override
    protected void initLogic(Context context, RemoteViews views) {
        RTConfig.initConfig(context);
        showMemoryInfo(context, false);
    }

    private void showMemoryInfo(Context context, boolean reinit) {
        MemoryInfo info = MemoryUtils.getMemoryInfo();
        int percent = 0;
        if (info != null) {
            percent = (int) (info.Used * 100.0F / info.Total);
        }
        if (reinit) {
            reinit(context);
        }
        setViewVisibility(context, R.id.wbtn_clean, View.VISIBLE);
        setViewVisibility(context, R.id.wbtn_clean_disabled, View.GONE);
        setViewText(context, R.id.wtv_mem_used, context.getString(R.string.widget_used_fmt, info != null ? info.Used : 0));
        setViewText(context, R.id.wtv_mem_free, context.getString(R.string.widget_free_fmt, info != null ? info.Free : 0));
        setViewText(context, R.id.wtv_mem_shared, context.getString(R.string.widget_share_fmt, info != null ? info.Shared : 0));
        setViewText(context, R.id.wtv_mem_buffered, context.getString(R.string.widget_buffer_fmt, info != null ? info.Buffer : 0));
        setViewText(context, R.id.wtv_mem_load_percent, String.format("%d%%", percent));
        if (percent > 90) {
            setViewTextColor(context, R.id.wtv_mem_load_percent, Color.RED);
        } else if (percent < 70) {
            setViewTextColor(context, R.id.wtv_mem_load_percent, Color.GREEN);
        } else {
            setViewTextColor(context, R.id.wtv_mem_load_percent, Color.YELLOW);
        }
        sync(context, RootToolsWidget4x1.class);
    }

    @Override
    public void onWidgetClick(Context context, String action) {
        if (action.equals(Actions.ACTION_WIDGET_CLEAN)) {
            if (!AutoCleanMemoryService.isAlive) {
                reinit(context);
                setViewVisibility(context, R.id.wbtn_clean, View.GONE);
                setViewVisibility(context, R.id.wbtn_clean_disabled, View.VISIBLE);
                sync(context, RootToolsWidget4x1.class);
                Toast.makeText(context, R.string.widget_cleaning, Toast.LENGTH_SHORT).show();
                Intent inClean = new Intent(context, AutoCleanMemoryService.class);
                inClean.putExtra("message", Actions.ACTION_WIDGET_CLEAN_FINISH);
                context.startService(inClean);
            }
        } else if (action.equals(Actions.ACTION_WIDGET_LAUNCH)) {
            ApkUtils.openApp(context, context.getPackageName(), true);
        } else if (action.equals(Actions.ACTION_WIDGET_CLEAN_FINISH)) {
            context.stopService(new Intent(context, AutoCleanMemoryService.class));
            Toast.makeText(context, R.string.widget_cleaning_finish, Toast.LENGTH_SHORT).show();
            showMemoryInfo(context, true);
        } else if (action.equals(Actions.ACTION_WIDGET_KEEP)) {
            reinit(context);
        }
    }

    @Override
    public void onEnabled(Context context) {
        super.onEnabled(context);
        context.startService(new Intent(context, WidgetKeepService.class));

    }

    @Override
    public void onDisabled(Context context) {
        context.stopService(new Intent(context, WidgetKeepService.class));
        super.onDisabled(context);
    }
}
