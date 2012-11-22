package com.rarnu.zoe.love2.comp;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.zoe.love2.R;

public class BottomButton extends RelativeLayout {

	ImageView ivIcon;
	TextView tvText;

	public BottomButton(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public BottomButton(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public BottomButton(Context context) {
		super(context);
		init();
	}

	private void init() {
		setLayoutParams(new RelativeLayout.LayoutParams(
				LayoutParams.WRAP_CONTENT, LayoutParams.MATCH_PARENT));
		View v = inflate(getContext(), R.layout.comp_bottom_button, null);
		v.setLayoutParams(new RelativeLayout.LayoutParams(
				LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
		addView(v);

		ivIcon = (ImageView) findViewById(R.id.ivIcon);
		tvText = (TextView) findViewById(R.id.tvText);
	}

	public void setIcon(int res) {
		ivIcon.setImageResource(res);
	}

	public void setText(int res) {
		tvText.setText(res);
	}

	public void setText(CharSequence text) {
		tvText.setText(text);
	}
	
	public void setTextColor(int color) {
		tvText.setTextColor(color);
	}
	
	public void setTextSize(float size) {
		tvText.setTextSize(size);
	}
}
