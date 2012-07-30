package com.rarnu.tools.root.base;

import android.os.Bundle;
import android.widget.Button;
import android.widget.TextView;

import com.baidu.mapapi.MapActivity;
import com.rarnu.tools.root.R;

public abstract class BaseMapActivity extends MapActivity implements ActivityIntf {

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
	
	@Override
	protected boolean isRouteDisplayed() {
		
		return false;
	}

}
