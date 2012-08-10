package com.rarnu.findaround.base;

import android.app.Activity;
import android.os.Bundle;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.findaround.R;

public class BaseActivity extends Activity {

	protected TextView tvName;
	protected ImageView btnLeft;
	protected Button btnRight;
	protected TextView backArea;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
	}

	protected void init() {
		mappingComponents();
	}

	protected void mappingComponents() {
		tvName = (TextView) findViewById(R.id.tvName);
		btnLeft = (ImageView) findViewById(R.id.btnLeft);
		btnRight = (Button) findViewById(R.id.btnRight);
		backArea = (TextView) findViewById(R.id.backArea);
	}
}
