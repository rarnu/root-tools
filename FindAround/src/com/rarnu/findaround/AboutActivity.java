package com.rarnu.findaround;

import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;

import com.rarnu.findaround.base.BaseActivity;

public class AboutActivity extends BaseActivity implements OnClickListener {

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.about);
		init();
	}
	
	@Override
	protected void init() {
		super.init();
		backArea.setOnClickListener(this);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.backArea:
			finish();
			break;
		}
	}
}
