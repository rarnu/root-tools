package com.rarnu.tools.root.comp;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import com.rarnu.root.pp4.R;

public class WelcomeButton extends RelativeLayout {

	// [region] const define
	public static final int BS_NONE = 0;
	public static final int BS_WARNING = 1;
	public static final int BS_UPDATE = 2;
	public static final int BS_BANNED = 3;

	// [/region]

	// [region] field define
	private Button btnWelcome;
	private ImageView imgInfo;

	// [/region]

	// [region] constructor
	public WelcomeButton(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public WelcomeButton(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public WelcomeButton(Context context) {
		super(context);
		init();
	}

	// [/region]

	// [region] common

	private void init() {
		addView(inflate(getContext(), R.layout.welcome_button, null));
		btnWelcome = (Button) findViewById(R.id.btnWelcome);
		imgInfo = (ImageView) findViewById(R.id.imgInfo);

	}

	public void setText(CharSequence text) {
		btnWelcome.setText(text);
	}

	public Button getButton() {
		return btnWelcome;
	}

	public void setButtonId(int id) {
		btnWelcome.setId(id);
	}

	public void setImageStyle(int style) {
		switch (style) {
		case BS_NONE:
			imgInfo.setVisibility(View.GONE);
			break;
		case BS_UPDATE:
			imgInfo.setBackgroundResource(R.drawable.new_version);
			imgInfo.setVisibility(View.VISIBLE);
			break;
		case BS_WARNING:
			imgInfo.setBackgroundResource(R.drawable.warning);
			imgInfo.setVisibility(View.VISIBLE);
			break;
		case BS_BANNED:
			imgInfo.setBackgroundResource(R.drawable.banned);
			imgInfo.setVisibility(View.VISIBLE);
			break;
		}
	}
	// [/region]
}
