package com.rarnu.tools.root.comp;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.Button;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.tools.root.R;

public class TitleBar extends RelativeLayout {

	// [region] field define
	private Button btnLeft, btnRight;
	private TextView tvName;

	// [/region]

	// [region] constructor
	public TitleBar(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public TitleBar(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public TitleBar(Context context) {
		super(context);
		init();
	}

	// [/region]

	// [region] common
	private void init() {
		addView(inflate(getContext(), R.layout.title_bar, null));
		btnLeft = (Button) findViewById(R.id.btnLeft);
		btnRight = (Button) findViewById(R.id.btnRight);
		tvName = (TextView) findViewById(R.id.tvName);
	}

	public Button getLeftButton() {
		return btnLeft;
	}

	public Button getRightButton() {
		return btnRight;
	}

	public void setText(CharSequence text) {
		tvName.setText(text);
	}

	public void setLeftButtonText(CharSequence text) {
		btnLeft.setText(text);
	}

	public void setRightButtonText(CharSequence text) {
		btnRight.setText(text);
	}
	// [/region]
}
