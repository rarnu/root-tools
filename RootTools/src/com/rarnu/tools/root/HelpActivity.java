package com.rarnu.tools.root;

import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.webkit.WebSettings;
import android.webkit.WebSettings.LayoutAlgorithm;
import android.webkit.WebView;

import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.BaseActivity;

public class HelpActivity extends BaseActivity implements OnClickListener {

	// [region] field define

	WebView wvHelp;

	// [/region]

	// [region] life circle
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_help);
		init();
		showHelp();
		LogApi.logEnterHelp();
	}

	// [/region]

	// [region] business logic
	private void showHelp() {
		WebSettings settings = wvHelp.getSettings();
		settings.setLoadWithOverviewMode(false);
		settings.setSupportZoom(false);
		settings.setAllowFileAccess(true);
		settings.setBuiltInZoomControls(false);
		settings.setLayoutAlgorithm(LayoutAlgorithm.SINGLE_COLUMN);

		wvHelp.setVerticalScrollBarEnabled(false);
		wvHelp.setHorizontalScrollBarEnabled(false);
		wvHelp.setFocusable(false);
		wvHelp.setFocusableInTouchMode(false);

		wvHelp.post(new Runnable() {

			@Override
			public void run() {
				wvHelp.loadUrl("file:///android_asset/help.html");
			}
		});
	}

	// [/region]

	// [region] events
	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLeft:
			finish();
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

		wvHelp = (WebView) findViewById(R.id.wvHelp);
	}

	@Override
	public void initTitle() {
		tvName.setText(R.string.app_help);
		btnLeft.setText(R.string.back);
		btnLeft.setVisibility(View.VISIBLE);
		// tbTitle.setText(getString(R.string.app_help));
		// tbTitle.setLeftButtonText(getString(R.string.back));
		// tbTitle.getLeftButton().setVisibility(View.VISIBLE);

	}

	@Override
	public void initSearchBar() {

	}

	@Override
	public void initEvents() {
		btnLeft.setOnClickListener(this);

	}

	// [/region]
}
