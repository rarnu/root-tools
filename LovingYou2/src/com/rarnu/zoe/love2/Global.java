package com.rarnu.zoe.love2;

import android.app.Activity;
import android.content.Context;

import com.rarnu.zoe.love2.api.LovingYouApi;
import com.rarnu.zoe.love2.database.DatabaseHelper;

public class Global {

	public static DatabaseHelper database = null;
	public static Activity activity = null;

	public static void initDatabase(Context context) {
		if (database == null) {
			database = new DatabaseHelper(context);
		}
	}
	
	public static void doUpdateTokenT(final Context context) {
		new Thread(new Runnable() {
			@Override
			public void run() {
				LovingYouApi.getToken(context);
			}
		}).start();
	}
}
