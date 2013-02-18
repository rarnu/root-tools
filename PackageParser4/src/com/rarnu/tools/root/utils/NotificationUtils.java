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

	public static void cancalAllNotification(final Context context,
			final int[] ids) {
		if (ids != null && ids.length != 0) {
			for (int id : ids) {
				cancelNotication(context, id);
			}
		}
	}

	public static void showNotification(final Context context, final int id,
			final int icon, final int title, final int desc,
			final String action, final boolean canClose) {
		NotificationManager manager = (NotificationManager) context
				.getSystemService(Context.NOTIFICATION_SERVICE);
		try {
			manager.cancel(id);
		} catch (Exception e) {

		}
		Notification n = buildNotification(context, id, icon, title, desc,
				action, canClose);
		manager.notify(id, n);
	}

	public static Notification buildNotification(final Context context,
			final int id, final int icon, final int title, final int desc,
			final String action, final boolean canClose) {
		Notification n = new Notification(icon, context.getString(title),
				System.currentTimeMillis());
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

		Intent inMain = new Intent(action);
		inMain.putExtra("id", id);
		Log.e("NotificationUtils", String.valueOf(id));
		PendingIntent pMain = PendingIntent.getBroadcast(context, 0, inMain,
				PendingIntent.FLAG_UPDATE_CURRENT);

		n.setLatestEventInfo(context, context.getString(title),
				context.getString(desc), pMain);
		return n;
	}

}
