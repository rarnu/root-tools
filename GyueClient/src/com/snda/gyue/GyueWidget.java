package com.snda.gyue;

import android.app.PendingIntent;
import android.appwidget.AppWidgetManager;
import android.appwidget.AppWidgetProvider;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.util.Log;
import android.view.View;
import android.widget.RemoteViews;

public class GyueWidget extends AppWidgetProvider {

	static PendingIntent piRefresh = null, piLaunch = null;
	static PendingIntent piItem1 = null, piItem2 = null, piItem3 = null, piItem4 = null, piItem5 = null, piItem6 = null, piItem7 = null, piItem8 = null,
			piItem9 = null, piItem10 = null;

	@Override
	public void onUpdate(Context context, AppWidgetManager appWidgetManager, int[] appWidgetIds) {
		super.onUpdate(context, appWidgetManager, appWidgetIds);
		setClickEvent(context);
		context.startService(new Intent(context, WidgetService.class));
	}

	public static void setClickEvent(Context context) {

		Log.e("GyueWidget", "setClickEvent");
		RemoteViews views = new RemoteViews(context.getPackageName(), R.layout.widget);

		Intent inRefresh = new Intent("com.snda.gyue.refresh_widget");
		piRefresh = PendingIntent.getBroadcast(context, 0, inRefresh, 0);
		views.setOnClickPendingIntent(R.id.imgRefresh, piRefresh);

		Intent inLaunch = new Intent("com.snda.gyue.launch");
		piLaunch = PendingIntent.getActivity(context, 0, inLaunch, 0);
		views.setOnClickPendingIntent(R.id.ivLaunchGyue, piLaunch);

		piItem1 = buildPendingIntent(context, 0);
		views.setOnClickPendingIntent(R.id.tvItem1, piItem1);

		piItem2 = buildPendingIntent(context, 1);
		views.setOnClickPendingIntent(R.id.tvItem2, piItem2);

		piItem3 = buildPendingIntent(context, 2);
		views.setOnClickPendingIntent(R.id.tvItem3, piItem3);

		piItem4 = buildPendingIntent(context, 3);
		views.setOnClickPendingIntent(R.id.tvItem4, piItem4);

		piItem5 = buildPendingIntent(context, 4);
		views.setOnClickPendingIntent(R.id.tvItem5, piItem5);

		piItem6 = buildPendingIntent(context, 5);
		views.setOnClickPendingIntent(R.id.tvItem6, piItem6);

		piItem7 = buildPendingIntent(context, 6);
		views.setOnClickPendingIntent(R.id.tvItem7, piItem7);

		piItem8 = buildPendingIntent(context, 7);
		views.setOnClickPendingIntent(R.id.tvItem8, piItem8);

		piItem9 = buildPendingIntent(context, 8);
		views.setOnClickPendingIntent(R.id.tvItem9, piItem9);

		piItem10 = buildPendingIntent(context, 9);
		views.setOnClickPendingIntent(R.id.tvItem10, piItem10);

		AppWidgetManager manager = AppWidgetManager.getInstance(context);
		int[] appids = manager.getAppWidgetIds(new ComponentName(context, GyueWidget.class));
		manager.updateAppWidget(appids, views);
	}

	public static PendingIntent buildPendingIntent(Context context, int id) {
		Intent inArticle = new Intent("com.snda.gyue.view_article." + String.valueOf(id));
		return PendingIntent.getBroadcast(context, id, inArticle, 0);
	}

	public static void setRefreshStatus(Context context, boolean refreshing) {
		Log.e("GyueWidget", "setRefreshStatus");
		RemoteViews views = new RemoteViews(context.getPackageName(), R.layout.widget);
		views.setViewVisibility(R.id.tvRefresh, (refreshing ? View.VISIBLE : View.GONE));
		views.setViewVisibility(R.id.imgRefresh, (refreshing ? View.GONE : View.VISIBLE));

		AppWidgetManager manager = AppWidgetManager.getInstance(context);
		int[] appids = manager.getAppWidgetIds(new ComponentName(context, GyueWidget.class));
		manager.updateAppWidget(appids, views);
	}

	public static RemoteViews getWidgetView(Context context) {
		RemoteViews views = new RemoteViews(context.getPackageName(), R.layout.widget);
		views.setViewVisibility(R.id.tvItem1, View.GONE);
		views.setViewVisibility(R.id.tvItem2, View.GONE);
		views.setViewVisibility(R.id.tvItem3, View.GONE);
		views.setViewVisibility(R.id.tvItem4, View.GONE);
		views.setViewVisibility(R.id.tvItem5, View.GONE);
		views.setViewVisibility(R.id.tvItem6, View.GONE);
		views.setViewVisibility(R.id.tvItem7, View.GONE);
		views.setViewVisibility(R.id.tvItem8, View.GONE);
		views.setViewVisibility(R.id.tvItem9, View.GONE);
		views.setViewVisibility(R.id.tvItem10, View.GONE);

		if (GlobalInstance.gListFocusedArticles != null) {
			if (GlobalInstance.gListFocusedArticles.size() >= 1) {
				views.setTextViewText(R.id.tvItem1, GlobalInstance.gListFocusedArticles.get(0).getTitle());
				views.setViewVisibility(R.id.tvItem1, View.VISIBLE);
			}
			if (GlobalInstance.gListFocusedArticles.size() >= 2) {
				views.setTextViewText(R.id.tvItem2, GlobalInstance.gListFocusedArticles.get(1).getTitle());
				views.setViewVisibility(R.id.tvItem2, View.VISIBLE);
			}
			if (GlobalInstance.gListFocusedArticles.size() >= 3) {
				views.setTextViewText(R.id.tvItem3, GlobalInstance.gListFocusedArticles.get(2).getTitle());
				views.setViewVisibility(R.id.tvItem3, View.VISIBLE);
			}
			if (GlobalInstance.gListFocusedArticles.size() >= 4) {
				views.setTextViewText(R.id.tvItem4, GlobalInstance.gListFocusedArticles.get(3).getTitle());
				views.setViewVisibility(R.id.tvItem4, View.VISIBLE);
			}
			if (GlobalInstance.gListFocusedArticles.size() >= 5) {
				views.setTextViewText(R.id.tvItem5, GlobalInstance.gListFocusedArticles.get(4).getTitle());
				views.setViewVisibility(R.id.tvItem5, View.VISIBLE);
			}
			if (GlobalInstance.gListFocusedArticles.size() >= 6) {
				views.setTextViewText(R.id.tvItem6, GlobalInstance.gListFocusedArticles.get(5).getTitle());
				views.setViewVisibility(R.id.tvItem6, View.VISIBLE);
			}
			if (GlobalInstance.gListFocusedArticles.size() >= 7) {
				views.setTextViewText(R.id.tvItem7, GlobalInstance.gListFocusedArticles.get(6).getTitle());
				views.setViewVisibility(R.id.tvItem7, View.VISIBLE);
			}
			if (GlobalInstance.gListFocusedArticles.size() >= 8) {
				views.setTextViewText(R.id.tvItem8, GlobalInstance.gListFocusedArticles.get(7).getTitle());
				views.setViewVisibility(R.id.tvItem8, View.VISIBLE);
			}
			if (GlobalInstance.gListFocusedArticles.size() >= 9) {
				views.setTextViewText(R.id.tvItem9, GlobalInstance.gListFocusedArticles.get(8).getTitle());
				views.setViewVisibility(R.id.tvItem9, View.VISIBLE);
			}
			if (GlobalInstance.gListFocusedArticles.size() >= 10) {
				views.setTextViewText(R.id.tvItem10, GlobalInstance.gListFocusedArticles.get(9).getTitle());
				views.setViewVisibility(R.id.tvItem10, View.VISIBLE);
			}
		}
		setClickEvent(context);
		return views;
	}

	@Override
	public void onReceive(Context context, Intent intent) {
		super.onReceive(context, intent);

		if (intent == null) {
			return;
		}
		String action = intent.getAction();
		if (action == null) {
			return;
		}
		if (action.equals("com.snda.gyue.refresh_widget")) {
			context.startService(new Intent(context, WidgetService.class));
		}

		if (action.contains("com.snda.gyue.view_article")) {
			if (GlobalInstance.gListFocusedArticles == null || GlobalInstance.gListFocusedArticles.size() == 0) {
				return;
			}
			int itemid = Integer.parseInt(action.substring(action.length() - 1));
			if (itemid >= GlobalInstance.gListFocusedArticles.size() - 1) {
				return;
			}
			Intent inArticle = new Intent("com.snda.gyue.view");
			inArticle.putExtra("mode", "widget");
			inArticle.putExtra("item", itemid);
			inArticle.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
			context.startActivity(inArticle);
		}
	}
}
