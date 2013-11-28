package com.rarnu.devlib.base;

import android.app.PendingIntent;
import android.appwidget.AppWidgetManager;
import android.appwidget.AppWidgetProvider;
import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.widget.RemoteViews;

public abstract class BaseAppWidget extends AppWidgetProvider {

    RemoteViews views = null;
    AppWidgetManager manager = null;

    protected abstract int getWidgetLayoutResId();

    protected abstract void initComponents(RemoteViews views);

    protected abstract void initEvents(RemoteViews views);

    protected abstract void initLogic(RemoteViews views);

    protected void initRemoteViews(Context context) {
        if (views == null) {
            views = new RemoteViews(context.getPackageName(), getWidgetLayoutResId());
        }
    }

    protected void updateRemoteViews(AppWidgetManager manager, int[] id) {
        if (views != null) {
            manager.updateAppWidget(id, views);
        }
    }

    @Override
    public void onUpdate(Context context, AppWidgetManager appWidgetManager, int[] appWidgetIds) {
        initRemoteViews(context);
        initComponents(views);
        initEvents(views);
        initLogic(views);
        updateRemoteViews(appWidgetManager, appWidgetIds);
    }

    /**
     * @param context
     * @param requestCode
     * @param mode        1:activity  2:service  3:broadcast
     * @param inEvent
     * @param viewId
     */
    public void registerEvent(Context context, int requestCode, int mode, Intent inEvent, int viewId) {
        initRemoteViews(context);
        PendingIntent pi = null;
        switch (mode) {
            case 1:
                pi = PendingIntent.getActivity(context, requestCode, inEvent, 0);
                break;
            case 2:
                pi = PendingIntent.getService(context, requestCode, inEvent, 0);
                break;
            case 3:
                pi = PendingIntent.getBroadcast(context, requestCode, inEvent, 0);
                break;
        }
        views.setOnClickPendingIntent(viewId, pi);
    }

    public void setViewText(Context context, int componentId, String text) {
        initRemoteViews(context);
        views.setTextViewText(componentId, text);
    }

    public void setViewImage(Context context, int componentId, Bitmap bmp) {
        initRemoteViews(context);
        views.setImageViewBitmap(componentId, bmp);
    }

    public void setViewProgress(Context context, int componentId, int max, int progress, boolean indeterminate) {
        initRemoteViews(context);
        views.setProgressBar(componentId, max, progress, indeterminate);
    }
}
