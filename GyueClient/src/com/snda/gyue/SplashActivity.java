package com.snda.gyue;

import java.util.Timer;
import java.util.TimerTask;

import android.app.Activity;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.view.View;

public class SplashActivity extends Activity {

	Timer tmrClose = new Timer();

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		GlobalInstance.aSplash = this;
		setContentView(R.layout.splash);

		SharedPreferences sp = PreferenceManager
				.getDefaultSharedPreferences(this);
		boolean firstStart = sp.getBoolean("firstStart", true);

		if (firstStart) {
			findViewById(R.id.tvFirstStart).setVisibility(View.VISIBLE);
		}

		tmrClose.schedule(new TimerTask() {
			@Override
			public void run() {
				finish();
			}
		}, (firstStart ? 6000 : 2000));
		sp.edit().putBoolean("firstStart", false).commit();
	}

	@Override
	protected void onDestroy() {
		tmrClose.cancel();
		GlobalInstance.aSplash = null;

		// show first time guide
		boolean firstMain = PreferenceManager.getDefaultSharedPreferences(this)
				.getBoolean("firstMain", true);
		if (firstMain) {
			Intent inGuide = new Intent(this, GuideActivity.class);
			inGuide.putExtra("img", 1);
			startActivity(inGuide);
			PreferenceManager.getDefaultSharedPreferences(this).edit()
					.putBoolean("firstMain", false).commit();
		}

		super.onDestroy();
	}

}
