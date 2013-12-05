package com.rarnu.devlib.base;

import android.app.PendingIntent;
import android.appwidget.AppWidgetManager;
import android.appwidget.AppWidgetProvider;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.util.Log;
import android.widget.RemoteViews;

public abstract class BaseAppWidget extends AppWidgetProvider {

    protected RemoteViews views;

    protected abstract int getWidgetLayoutResId();

    protected abstract void initComponents(Context context, RemoteViews views);

    protected abstract void initEvents(Context context, RemoteViews views);

    protected abstract void initLogic(Context context, RemoteViews views);

    public abstract void onWidgetClick(Context context, String action);

    protected void reinit(Context context) {
        views = new RemoteViews(context.getPackageName(), getWidgetLayoutResId());
    }

    protected void sync(Context context, Class<?> clz) {
        ComponentName cn = new ComponentName(context, clz);
        AppWidgetManager.getInstance(context).updateAppWidget(cn, views);
    }

    @Override
    public void onUpdate(Context context, AppWidgetManager appWidgetManager, int[] appWidgetIds) {
        views = new RemoteViews(context.getPackageName(), getWidgetLayoutResId());
        init(context, views);
        appWidgetManager.updateAppWidget(appWidgetIds, views);
    }

    public void reUpdate(Context context, Class<?> clz) {
        views = new RemoteViews(context.getPackageName(), getWidgetLayoutResId());
        AppWidgetManager manager = AppWidgetManager.getInstance(context);
        ComponentName cn = new ComponentName(context, clz);
        init(context, views);
        manager.updateAppWidget(cn, views);
    }

    private void init(Context context, RemoteViews views) {
        initComponents(context, views);
        initEvents(context, views);
        initLogic(context, views);
    }

    @Override
    public void onReceive(Context context, Intent intent) {
        super.onReceive(context, intent);
        String action = intent.getAction();
        if (action != null) {
            if (!action.equals(AppWidgetManager.ACTION_APPWIDGET_UPDATE)
                    && !action.equals(AppWidgetManager.ACTION_APPWIDGET_DELETED)
                    && !action.equals(AppWidgetManager.ACTION_APPWIDGET_ENABLED)
                    && !action.equals(AppWidgetManager.ACTION_APPWIDGET_OPTIONS_CHANGED)
                    && !action.equals(AppWidgetManager.ACTION_APPWIDGET_DISABLED)) {
                onWidgetClick(context, action);
            }
        }

    }

    /**
     * @param context
     * @param mode    1:activity  2:service  3:broadcast
     * @param inEvent
     * @param viewId
     */
    public void registerEvent(Context context, int mode, Intent inEvent, int viewId) {
        PendingIntent pi = null;
        switch (mode) {
            case 1:
                pi = PendingIntent.getActivity(context, 0, inEvent, 0);
                break;
            case 2:
                pi = PendingIntent.getService(context, 0, inEvent, 0);
                break;
            case 3:
                pi = PendingIntent.getBroadcast(context, 0, inEvent, 0);
                break;
        }
        views.setOnClickPendingIntent(viewId, pi);
    }

    /**
     * @param context
     * @param action  must be registered in AndroidManifest
     * @param viewId
     */
    public void registerEvent(Context context, String action, int viewId) {

        Intent inClick = new Intent(action);
        PendingIntent pi = PendingIntent.getBroadcast(context, 0, inClick, 0);
        views.setOnClickPendingIntent(viewId, pi);
    }

    public void setViewText(Context context, int componentId, String text) {
        views.setTextViewText(componentId, text);
    }

    public void setViewImage(Context context, int componentId, Bitmap bmp) {
        views.setImageViewBitmap(componentId, bmp);
    }

    public void setViewProgress(Context context, int componentId, int max, int progress, boolean indeterminate) {
        views.setProgressBar(componentId, max, progress, indeterminate);
    }

    public void setViewVisibility(Context context, int componentId, int visibility) {
        views.setViewVisibility(componentId, visibility);
    }

    public void setViewTextColor(Context context, int componentId, int color) {
        views.setTextColor(componentId, color);
    }
}
