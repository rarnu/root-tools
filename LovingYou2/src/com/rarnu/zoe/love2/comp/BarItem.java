package com.rarnu.zoe.love2.comp;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.zoe.love2.R;

public class BarItem extends RelativeLayout {

	ImageView ivIcon;
	TextView tvTitle;

	public BarItem(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public BarItem(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public BarItem(Context context) {
		super(context);
		init();
	}

	private void init() {
		setLayoutParams(new RelativeLayout.LayoutParams(
				LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT));
		View v = inflate(getContext(), R.layout.comp_baritem, null);
		v.setLayoutParams(new RelativeLayout.LayoutParams(
				LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT));
		addView(v);

		ivIcon = (ImageView) findViewById(R.id.ivIcon);
		tvTitle = (TextView) findViewById(R.id.tvTitle);
	}

	public void setIcon(int res) {
		ivIcon.setImageResource(res);
	}

	public void setText(int res) {
		tvTitle.setText(res);
	}

	public void setText(CharSequence text) {
		tvTitle.setText(text);
	}
	
	public void setTextColor(int color) {
		tvTitle.setTextColor(color);
	}
	
	public void setTextSize(float size) {
		tvTitle.setTextSize(size);
	}
}
