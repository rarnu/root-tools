package com.zoe.calendar.fragment;

import android.os.Bundle;
import android.view.Menu;
import android.webkit.WebChromeClient;
import android.webkit.WebSettings;
import android.webkit.WebSettings.LayoutAlgorithm;
import android.webkit.WebView;
import android.webkit.WebViewClient;

import com.rarnu.devlib.base.BaseFragment;
import com.zoe.calendar.R;
import com.zoe.calendar.classes.ActivityItem;
import com.zoe.calendar.utils.ResourceUtils;

public class DetailWebFragment extends BaseFragment {

	ActivityItem actItem;

	WebView wvUrl;

	public DetailWebFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.fragment_detail_web);
		tabTitle = ResourceUtils.getString(R.string.menu_url);
	}

	public DetailWebFragment(String tag, String title) {
		super(tag, title);
	}

	@Override
	public int getBarTitle() {
		return R.string.detail_name;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.detail_name;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		wvUrl = (WebView) innerView.findViewById(R.id.wvUrl);
		WebSettings ws = wvUrl.getSettings();
		ws.setAllowFileAccess(true);
		ws.setAppCacheEnabled(true);
		ws.setCacheMode(WebSettings.LOAD_CACHE_ELSE_NETWORK);
		ws.setBuiltInZoomControls(false);
		ws.setLayoutAlgorithm(LayoutAlgorithm.SINGLE_COLUMN);
		ws.setDisplayZoomControls(false);
		ws.setJavaScriptEnabled(true);
		ws.setSupportZoom(false);
		ws.setDefaultTextEncodingName("GBK");

		wvUrl.setWebViewClient(new WebViewClient() {
			@Override
			public boolean shouldOverrideUrlLoading(final WebView view,
					final String url) {
				view.post(new Runnable() {

					@Override
					public void run() {
						view.loadUrl(url);
					}
				});
				return false;
			}
		});
		wvUrl.setWebChromeClient(new WebChromeClient());

	}

	@Override
	public void initEvents() {

	}

	@Override
	public void initLogic() {
		actItem = (ActivityItem) getActivity().getIntent()
				.getSerializableExtra("item");
		wvUrl.post(new Runnable() {

			@Override
			public void run() {
				wvUrl.loadUrl(actItem.url);
			}
		});
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_detail_web;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {

	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

}
