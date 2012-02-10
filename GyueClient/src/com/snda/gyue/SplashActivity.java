package com.snda.gyue;

import android.app.Activity;
import android.os.Bundle;

public class SplashActivity extends Activity {

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		GlobalInstance.aSplash = this;
		setContentView(R.layout.splash);
	}
	
	@Override
	protected void onDestroy() {
		GlobalInstance.aSplash = null;
		super.onDestroy();
	}

}
