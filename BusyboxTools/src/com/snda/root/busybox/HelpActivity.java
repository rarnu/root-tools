package com.snda.root.busybox;

import android.app.Activity;
import android.os.Bundle;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;

public class HelpActivity extends Activity {

	WebView wvHelp;

	@Override
	public void onCreate(Bundle savedInstanceState) {

		super.onCreate(savedInstanceState);

		setContentView(R.layout.help);
		
		wvHelp = (WebView) findViewById(R.id.wvHelp);

		WebViewClient client = new WebViewClient();
		wvHelp.setWebViewClient(client);

		WebSettings settings = wvHelp.getSettings();
		settings.setAllowFileAccess(true);
		settings.setLoadWithOverviewMode(false);
		settings.setUseWideViewPort(true);
		settings.setSupportZoom(false);

		wvHelp.setVerticalScrollBarEnabled(true);
		wvHelp.setHorizontalScrollBarEnabled(false);
		wvHelp.setClickable(false);
		wvHelp.loadUrl("file:///android_asset/help.html");

	}
}
