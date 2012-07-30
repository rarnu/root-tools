package com.rarnu.findaround.base;

import com.rarnu.findaround.R;

import android.app.Activity;
import android.os.Bundle;
import android.widget.Button;
import android.widget.TextView;

public class BaseActivity extends Activity {

	protected TextView tvName;
	protected Button btnLeft, btnRight;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
	}

	protected void init() {
		mappingComponents();
	}

	protected void mappingComponents() {
		tvName = (TextView) findViewById(R.id.tvName);
		btnLeft = (Button) findViewById(R.id.btnLeft);
		btnRight = (Button) findViewById(R.id.btnRight);
	}
}
