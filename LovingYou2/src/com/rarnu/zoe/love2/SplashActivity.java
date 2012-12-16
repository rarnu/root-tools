package com.rarnu.zoe.love2;

import android.app.Activity;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;

public class SplashActivity extends Activity implements OnClickListener {
	
	ImageView btnStart;
	ImageView img1, img2, img3;
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_splash);
		img1 = (ImageView) findViewById(R.id.img1);
		img2 = (ImageView) findViewById(R.id.img2);
		img3 = (ImageView) findViewById(R.id.img3);
		
		img1.setImageResource(R.drawable.splash1);
		img2.setImageResource(R.drawable.splash2);
		img3.setImageResource(R.drawable.splash3);
		
		
		btnStart = (ImageView) findViewById(R.id.btnStart);
		btnStart.setOnClickListener(this);
	}
	
	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event) {
		if (keyCode == KeyEvent.KEYCODE_BACK) {
			return true;
		}
		return super.onKeyDown(keyCode, event);
	}

	@Override
	public void onClick(View v) {
		finish();
	}
}
