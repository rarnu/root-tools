package com.snda.root.datamgr;

import android.app.TabActivity;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.RelativeLayout;
import android.widget.TabHost;
import android.widget.TabWidget;
import android.widget.TabHost.TabSpec;

public class HelpActivity extends TabActivity {

	TabHost tabHost;
	TabSpec tabHelp, tabAbout;
	
	WebView wvHelp;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		tabHost = getTabHost();

		LayoutInflater.from(this).inflate(R.layout.help,
				tabHost.getTabContentView(), true);
		tabHelp = inflateTab(R.id.tab_help, "id_help", getResources().getString(
				R.string.tab_help_title), R.drawable.icon);
		tabAbout = inflateTab(R.id.tab_about, "id_about", getResources()
				.getString(R.string.tab_about_title), R.drawable.icon);

		tabHost.addTab(tabHelp);
		tabHost.addTab(tabAbout);

		setTabHost();
		
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

	private TabSpec inflateTab(int resource, String id, String title, int icon) {
		return tabHost.newTabSpec(id).setIndicator(title, null).setContent(
				resource);
	}

	private void setTabHost() {
		TabWidget tw = tabHost.getTabWidget();

		ViewGroup.LayoutParams vglp = tw.getLayoutParams();
		vglp.height = 48;
		tw.setLayoutParams(vglp);

		for (int i = 0; i < tw.getChildCount(); i++) {
			RelativeLayout rl = (RelativeLayout) tw.getChildTabViewAt(i);
			rl.getLayoutParams().height = 48;
		}
		tw.invalidate();

	}
}
