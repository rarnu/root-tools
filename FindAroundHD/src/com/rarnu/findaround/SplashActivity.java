package com.rarnu.findaround;

import java.util.Timer;
import java.util.TimerTask;

import android.os.Bundle;
import android.view.KeyEvent;

import com.rarnu.findaround.base.BaseActivity;

public class SplashActivity extends BaseActivity {

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.splash);

		final Timer tmr = new Timer();
		tmr.schedule(new TimerTask() {

			@Override
			public void run() {
				tmr.cancel();
				finish();

			}
		}, 2500);
	}

	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event) {
		if (keyCode == KeyEvent.KEYCODE_BACK) {
			return true;
		}
		return super.onKeyDown(keyCode, event);
	}
}
