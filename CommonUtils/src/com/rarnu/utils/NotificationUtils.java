package com.rarnu.utils;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;

import java.io.Serializable;

public class NotificationUtils {

    public static void cancelNotication(final Context context, final int id) {
        NotificationManager manager = (NotificationManager) context.getSystemService(Context.NOTIFICATION_SERVICE);
        try {
            manager.cancel(id);
        } catch (Exception e) {

        }
    }

    public static void cancalAllNotification(final Context context, final int[] ids) {
        if (ids != null && ids.length != 0) {
            for (int id : ids) {
                cancelNotication(context, id);
            }
        }
    }

    public static void showNotification(final Context context, final int id, final int icon, final int title, final int desc, final String action, final Serializable object, final boolean canClose) {
        NotificationManager manager = (NotificationManager) context.getSystemService(Context.NOTIFICATION_SERVICE);
        try {
            manager.cancel(id);
        } catch (Exception e) {

        }
        Notification n = buildNotification(context, id, icon, title, desc, action, object, canClose);
        manager.notify(id, n);
    }

    public static void showNotification(final Context context, final int id, final int icon, final int title, final int desc, final String action, final boolean canClose) {
        showNotification(context, id, icon, title, desc, action, null, canClose);
    }

    public static Notification buildNotification(final Context context, final int id, final int icon, final int title, final int desc, final String action, Serializable object, final boolean canClose) {

        Intent inMain = new Intent(action);
        inMain.putExtra("id", id);
        if (object != null) {
            inMain.putExtra("object", object);
        }
        PendingIntent pMain = PendingIntent.getBroadcast(context, 0, inMain, PendingIntent.FLAG_UPDATE_CURRENT);

        Notification n = new Notification.Builder(context)
                .setSmallIcon(icon)
                .setTicker(context.getString(title))
                .setWhen(System.currentTimeMillis())
                .setContentTitle(context.getString(title))
                .setContentText(context.getString(desc))
                .setContentIntent(pMain)
                .getNotification();

        if (canClose) {
            n.defaults |= Notification.DEFAULT_SOUND;
        }
        n.defaults |= Notification.DEFAULT_LIGHTS;
        n.ledARGB = 0xff00ff00;
        n.ledOnMS = 300;
        n.ledOffMS = 1000;
        n.flags |= Notification.FLAG_SHOW_LIGHTS;
        if (!canClose) {
            n.flags |= Notification.FLAG_ONGOING_EVENT;
        }

        return n;
    }

}
