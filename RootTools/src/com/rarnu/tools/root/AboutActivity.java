package com.rarnu.tools.root;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.api.MobileApi;
import com.rarnu.tools.root.base.BaseActivity;
import com.rarnu.tools.root.comp.AlertDialogEx;
import com.rarnu.tools.root.utils.DeviceUtils;

public class AboutActivity extends BaseActivity implements OnClickListener {

	// [region] field define

	RelativeLayout layHelp, layFitable, layUpdate;
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
//			fitableClick++;
//			if (fitableClick > 10) {
//				fitableClick = 0;
//				Intent inEgg = new Intent(this, EggActivity.class);
//				startActivity(inEgg);
//			}
			break;
		case R.id.layUpdate:
			showUpdateInfo();
			break;
		}
	}

	// [/region]

	// [region] business logic

	private void showUpdateInfo() {

		if (GlobalInstance.updateInfo == null
				|| GlobalInstance.updateInfo.result == 0) {
			AlertDialogEx.showAlertDialogEx(this,
					getString(R.string.check_update),
					getString(R.string.no_update_found),
					getString(R.string.ok), null, null, null);
		} else {
			AlertDialogEx.showAlertDialogEx(this,
					getString(R.string.check_update), String.format(
							getString(R.string.update_found_info),
							GlobalInstance.updateInfo.versionName,
							GlobalInstance.updateInfo.size),
					getString(R.string.ok),
					new AlertDialogEx.DialogButtonClickListener() {

						@Override
						public void onClick(View v) {
							// download new version
							String downUrl = MobileApi.DOWNLOAD_BASE_URL
									+ GlobalInstance.updateInfo.file;
							Intent inDownload = new Intent(Intent.ACTION_VIEW);
							inDownload.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
							inDownload.setData(Uri.parse(downUrl));
							startActivity(inDownload);
						}
					}, getString(R.string.cancel), null);
		}
	}

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
		mappingTitle();
		mappingComp();
		initTitle();
		initSearchBar();
		initEvents();
	}

	@Override
	public void mappingComp() {

		layHelp = (RelativeLayout) findViewById(R.id.layHelp);
		layFitable = (RelativeLayout) findViewById(R.id.layFitable);
		imgFitable = (ImageView) findViewById(R.id.imgFitable);
		tvAppVersion = (TextView) findViewById(R.id.tvAppVersion);
		tvDebug = (TextView) findViewById(R.id.tvDebug);
		layUpdate = (RelativeLayout) findViewById(R.id.layUpdate);
	}

	@Override
	public void initTitle() {
		tvName.setText(R.string.about);
		btnLeft.setText(R.string.back);
		btnLeft.setVisibility(View.VISIBLE);
		// tbTitle.setText(getString(R.string.about));
		// tbTitle.setLeftButtonText(getString(R.string.back));
		// tbTitle.getLeftButton().setVisibility(View.VISIBLE);

	}

	@Override
	public void initSearchBar() {

	}

	@Override
	public void initEvents() {
		layHelp.setOnClickListener(this);
		layFitable.setOnClickListener(this);
		layUpdate.setOnClickListener(this);

		btnLeft.setOnClickListener(this);
		// tbTitle.getLeftButton().setOnClickListener(this);
	}

	// [/region]

}
