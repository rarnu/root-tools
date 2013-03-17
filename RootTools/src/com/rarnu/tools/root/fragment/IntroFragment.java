package com.rarnu.tools.root.fragment;

import java.util.Locale;

import android.os.Bundle;
import android.view.Menu;
import android.webkit.WebSettings;
import android.webkit.WebSettings.LayoutAlgorithm;
import android.webkit.WebView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;

public class IntroFragment extends BaseFragment {

	WebView wvHelp;

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
				String lang = Locale.getDefault().getLanguage();
				String country = Locale.getDefault().getCountry();

				String introUrl = "";
				try {
					if (lang.equals("zh")) {
						if (country.equals("TW")) {
							introUrl = "file:///android_asset/help_zn_TW.html";
						} else {
							introUrl = "file:///android_asset/help_zn_CN.html";
						}
					} else {
						introUrl = "file:///android_asset/help.html";
					}
				} catch (Exception e) {

				}
				if (!introUrl.equals("")) {
					wvHelp.loadUrl(introUrl);
				}
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

	@Override
	protected void initMenu(Menu menu) {

	}

	@Override
	protected void initLogic() {
		showHelp();

	}

	@Override
	protected void initEvents() {

	}

	@Override
	protected String getMainActivityName() {
		return MainActivity.class.getName();
	}

	@Override
	protected void onGetNewArguments(Bundle bn) {

	}

}
