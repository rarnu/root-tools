package com.rarnu.tools.root.fragment;

import android.os.Bundle;
import android.webkit.WebSettings;
import android.webkit.WebSettings.LayoutAlgorithm;
import android.webkit.WebView;

import com.rarnu.tools.root.R;
import com.rarnu.tools.root.base.BaseFragment;

public class IntroFragment extends BaseFragment {

	WebView wvHelp;
	
	@Override
	public void onActivityCreated(Bundle savedInstanceState) {
		super.onActivityCreated(savedInstanceState);
		showHelp();
	}

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

	@Override
	protected int getBarTitle() {
		return R.string.help;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.help_with_path;
	}

	@Override
	protected void initComponents() {
		wvHelp = (WebView) innerView.findViewById(R.id.wvHelp);
		
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.layout_help;
	}
}
