package com.snda.gyue;

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.RelativeLayout;

public class GuideActivity extends Activity implements OnClickListener {

	RelativeLayout layGuide;
	Button btnStart;
	
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.guide);

		int img = getIntent().getIntExtra("img", 1);
		layGuide = (RelativeLayout) findViewById(R.id.layGuide);
		layGuide.setBackgroundResource(img == 1 ? R.drawable.main_guide
				: R.drawable.view_guide);
		
		btnStart = (Button) findViewById(R.id.btnStart);
		btnStart.setOnClickListener(this);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnStart:
			finish();
			break;
		}
		
	}
}
