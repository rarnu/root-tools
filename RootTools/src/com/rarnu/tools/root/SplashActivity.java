package com.rarnu.tools.root;

import java.util.Timer;
import java.util.TimerTask;

import android.app.Activity;
import android.os.Bundle;
import android.view.KeyEvent;

public class SplashActivity extends Activity {

	// [region] life circle
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_splash);
		
		final Timer tmrClose = new Timer();
		tmrClose.schedule(new TimerTask() {
			
			@Override
			public void run() {
				tmrClose.cancel();
				finish();
			}
		}, 2000);
		
	}
	
	// [/region]
	
	// [region] events
	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event) {
		return true;
	}
	// [/region]


}
