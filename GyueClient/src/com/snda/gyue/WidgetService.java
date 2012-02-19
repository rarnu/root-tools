package com.snda.gyue;

import com.snda.gyue.network.HttpProxy;
import com.snda.gyue.network.ItemBuilder;
import com.snda.gyue.utils.MiscUtils;

import android.app.AlarmManager;
import android.app.PendingIntent;
import android.app.Service;
import android.appwidget.AppWidgetManager;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.os.Handler;
import android.os.IBinder;
import android.os.Message;
import android.util.Log;
import android.widget.RemoteViews;

public class WidgetService extends Service {

	@Override
	public IBinder onBind(Intent intent) {
		return null;
	}

	@Override
	public int onStartCommand(final Intent intent, int flags, int startId) {
		
		Log.e("GyueWidget", "updated");
		
		GyueWidget.setRefreshStatus(this, true);

		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					AppWidgetManager manager = AppWidgetManager.getInstance(WidgetService.this);
					RemoteViews views = GyueWidget.getWidgetView(WidgetService.this);
					int[] appids = manager.getAppWidgetIds(new ComponentName(WidgetService.this, GyueWidget.class));
					manager.updateAppWidget(appids, views);
					
					GyueWidget.setRefreshStatus(WidgetService.this, false);

					long now = System.currentTimeMillis();
					long unit = 1800000;

					PendingIntent pintent = PendingIntent.getService(WidgetService.this, 0, intent, 0);

					AlarmManager alarm = (AlarmManager) getSystemService(Context.ALARM_SERVICE);
					alarm.set(AlarmManager.RTC_WAKEUP, now + unit, pintent);
				}
				super.handleMessage(msg);
			}

		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				try {
					boolean local = (MiscUtils.getNetworkType(WidgetService.this) == 0);
					String xml = "";
					if (!local) {
						xml = HttpProxy.CallGet(GyueConsts.SITE_URL, String.format(GyueConsts.REQ_PARAMS, 13, 1, GyueConsts.PAGE_SIZE), "GBK");
					}
					GlobalInstance.gListFocusedArticles = ItemBuilder.xmlToItems(WidgetService.this, 13, xml, local, true);
				} catch (Exception e) {
					GlobalInstance.gListFocusedArticles = null;
				}
				h.sendEmptyMessage(1);
			}
		}).start();
		
		return super.onStartCommand(intent, flags, startId);
	}

}