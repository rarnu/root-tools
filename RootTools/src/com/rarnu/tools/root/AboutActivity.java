package com.rarnu.tools.root;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.ActivityIntf;
import com.rarnu.tools.root.comp.TitleBar;
import com.rarnu.tools.root.utils.DeviceUtils;

public class AboutActivity extends Activity implements ActivityIntf, OnClickListener {

	// [region] field define
	TitleBar tbTitle;
	RelativeLayout layHelp, layFitable;
	ImageView imgFitable;
	TextView tvAppVersion, tvDebug;
	// [/region]

	// [region] variable define
	int fitable = 5;
	int fitableClick = 0;
	// [/region]

	// [region] life circle
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_about);

		init();
		showAppVersion();
		showSystemFitable();
		showDebugStatus();
		LogApi.logEnterAbout();
		fitableClick = 0;
	}

	// [/region]

	// [region] events
	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLeft:
			finish();
			break;
		case R.id.layHelp:
			Intent inHelp = new Intent(this, HelpActivity.class);
			startActivity(inHelp);
			break;
		case R.id.layFitable:
			fitableClick++;
			if (fitableClick > 10) {
				fitableClick = 0;
				Intent inEgg = new Intent(this, EggActivity.class);
				startActivity(inEgg);
			}
			break;
		}
	}

	// [/region]

	// [region] business logic

	private void showDebugStatus() {
		tvDebug.setVisibility(GlobalInstance.DEBUG ? View.VISIBLE : View.GONE);
	}

	private void showAppVersion() {
		tvAppVersion.setText(DeviceUtils.getAppVersionName(this));
	}

	private void showSystemFitable() {

		fitable = DeviceUtils.getFitable();
		if (fitable < 1) {
			fitable = 1;
		}
		if (fitable > 9) {
			fitable = 9;
		}
		switch (fitable) {
		case 1:
			imgFitable.setBackgroundResource(R.drawable.c1);
			break;
		case 2:
			imgFitable.setBackgroundResource(R.drawable.c2);
			break;
		case 3:
			imgFitable.setBackgroundResource(R.drawable.c3);
			break;
		case 4:
			imgFitable.setBackgroundResource(R.drawable.c4);
			break;
		case 5:
			imgFitable.setBackgroundResource(R.drawable.c5);
			break;
		case 6:
			imgFitable.setBackgroundResource(R.drawable.c6);
			break;
		case 7:
			imgFitable.setBackgroundResource(R.drawable.c7);
			break;
		case 8:
			imgFitable.setBackgroundResource(R.drawable.c8);
			break;
		case 9:
			imgFitable.setBackgroundResource(R.drawable.c9);
			break;
		}
	}

	// [/region]

	// [region] init
	@Override
	public void init() {
		mappingComp();
		initTitle();
		initSearchBar();
		initEvents();
	}

	@Override
	public void mappingComp() {
		tbTitle = (TitleBar) findViewById(R.id.tbTitle);
		layHelp = (RelativeLayout) findViewById(R.id.layHelp);
		layFitable = (RelativeLayout) findViewById(R.id.layFitable);
		imgFitable = (ImageView) findViewById(R.id.imgFitable);
		tvAppVersion = (TextView) findViewById(R.id.tvAppVersion);
		tvDebug = (TextView) findViewById(R.id.tvDebug);
	}

	@Override
	public void initTitle() {
		tbTitle.setText(getString(R.string.about));
		tbTitle.setLeftButtonText(getString(R.string.back));
		tbTitle.getLeftButton().setVisibility(View.VISIBLE);

	}

	@Override
	public void initSearchBar() {

	}

	@Override
	public void initEvents() {
		layHelp.setOnClickListener(this);
		layFitable.setOnClickListener(this);
		tbTitle.getLeftButton().setOnClickListener(this);
	}

	// [/region]

}
