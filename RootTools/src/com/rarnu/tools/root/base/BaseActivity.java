package com.rarnu.tools.root.base;

import com.rarnu.tools.root.R;

import android.app.Activity;
import android.os.Bundle;
import android.widget.Button;
import android.widget.TextView;

public abstract class BaseActivity extends Activity implements ActivityIntf {

	protected TextView tvName;
	protected Button btnLeft, btnRight;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		
	}
	
	protected void mappingTitle() {
		tvName = (TextView) findViewById(R.id.tvName);
		btnLeft = (Button) findViewById(R.id.btnLeft);
		btnRight = (Button) findViewById(R.id.btnRight);
	}
}
