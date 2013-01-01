package com.rarnu.tools.root.comp;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.root.pp4.R;

public class DataProgressBar extends RelativeLayout {

	// [region] field define
	TextView tvProgressApp;
	TextView tvProgressCount;
	// [/region]

	// [region] constructor
	public DataProgressBar(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public DataProgressBar(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public DataProgressBar(Context context) {
		super(context);
		init();
	}

	// [/region]
	
	// [region] common
	public void init() {
		addView(inflate(getContext(), R.layout.dataapp_progress, null));
		tvProgressApp = (TextView) findViewById(R.id.tvProgressApp);
		tvProgressCount = (TextView) findViewById(R.id.tvProgressCount);
	}
	
	public void setAppName(CharSequence text) {
		tvProgressApp.setText(text);
	}
	
	public void setProgress(CharSequence text) {
		tvProgressCount.setText(text);
	}

	// [/region]
}
