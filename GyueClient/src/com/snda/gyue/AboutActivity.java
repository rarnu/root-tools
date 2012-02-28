package com.snda.gyue;

import com.snda.gyue.utils.UIUtils;

import android.app.Activity;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.Window;
import android.widget.TextView;

public class AboutActivity extends Activity implements OnClickListener {

	TextView tvGLink;
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		
		requestWindowFeature(Window.FEATURE_NO_TITLE);
		
		setContentView(R.layout.about);
		
		tvGLink = (TextView) findViewById(R.id.tvGLink);
		tvGLink.setOnClickListener(this);
	}
	
	@Override
	public boolean onTouchEvent(MotionEvent event) {
		if (!UIUtils.touchInDialog(this, event)) {
			finish();
		}
		return super.onTouchEvent(event);
	}
	
	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.tvGLink:
			Intent inWeb = new Intent(Intent.ACTION_VIEW);
			inWeb.setData(Uri.parse("http://www.gyue.cn"));
			startActivity(inWeb);
			break;
		}
		
	}
}
