package com.rarnu.findaround.base;

import android.os.Bundle;
import android.widget.Button;
import android.widget.TextView;

import com.baidu.mapapi.MapActivity;
import com.rarnu.findaround.R;

public class BaseMapActivity extends MapActivity {

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

	@Override
	protected boolean isRouteDisplayed() {
		// TODO Auto-generated method stub
		return false;
	}
}
