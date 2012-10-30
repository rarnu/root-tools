package com.rarnu.findaround;

import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.TextView;

import com.rarnu.findaround.base.BaseActivity;
import com.rarnu.findaround.common.DeviceUtils;

public class AboutActivity extends BaseActivity implements OnClickListener {

	TextView tvVersion;

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
		tvVersion.setText(String.format(getString(R.string.version_fmt),
				DeviceUtils.getAppVersionName(this)));
	}

	@Override
	protected void mappingComponents() {
		super.mappingComponents();
		tvVersion = (TextView) findViewById(R.id.tvVersion);
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
