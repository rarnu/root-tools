package com.rarnu.findaround.comp;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.webkit.WebSettings;
import android.webkit.WebView;

public class LavenderView extends WebView {

	public LavenderView(Context context) {
		super(context);
		setDefaultProperties();

	}

	public LavenderView(Context context, AttributeSet attrs) {
		super(context, attrs);
		setDefaultProperties();
	}

	public LavenderView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		setDefaultProperties();
	}

	public void setDefaultProperties() {
		setScrollBarStyle(View.SCROLLBARS_INSIDE_OVERLAY);
		WebSettings settings = getSettings();
		settings.setJavaScriptEnabled(true);
		settings.setAllowFileAccess(true);
		settings.setLoadWithOverviewMode(false);
		settings.setUseWideViewPort(true);
		settings.setSupportZoom(false);

		setVerticalScrollBarEnabled(false);
		setHorizontalScrollBarEnabled(false);
		setFocusable(true);
		setFocusableInTouchMode(true);
	}
}
