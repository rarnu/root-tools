package com.rarnu.tools.root.utils;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.util.Log;

public class NotificationUtils {

	public static void cancelNotication(final Context context, final int id) {
		NotificationManager manager = (NotificationManager) context
				.getSystemService(Context.NOTIFICATION_SERVICE);
		try {
			manager.cancel(id);
		} catch (Exception e) {

		}
	}

	public static void showNotification(final Context context, final int id,
			final int icon, final int title, final int desc, final String action) {
		NotificationManager manager = (NotificationManager) context
				.getSystemService(Context.NOTIFICATION_SERVICE);
		try {
			manager.cancel(id);
		} catch (Exception e) {

		}
		Notification n = new Notification(icon, context.getString(title),
				System.currentTimeMillis());

		n.defaults |= Notification.DEFAULT_SOUND;
		n.defaults |= Notification.DEFAULT_LIGHTS;
		n.ledARGB = 0xff00ff00;
		n.ledOnMS = 300;
		n.ledOffMS = 1000;
		n.flags |= Notification.FLAG_SHOW_LIGHTS;

		Intent inMain = new Intent(action);
		inMain.putExtra("id", id);
		Log.e("NotificationUtils", String.valueOf(id));
		PendingIntent pMain = PendingIntent.getBroadcast(context, 0, inMain,
				PendingIntent.FLAG_UPDATE_CURRENT);
		n.setLatestEventInfo(context, context.getString(title),
				context.getString(desc), pMain);
		manager.notify(id, n);
	}
}
