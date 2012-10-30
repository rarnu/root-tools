package com.rarnu.zoe.loving.comp;

import android.content.Context;
import android.graphics.Color;
import android.util.AttributeSet;
import android.view.Gravity;
import android.view.View;
import android.widget.Button;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.zoe.loving.R;
import com.rarnu.zoe.loving.utils.UIUtils;

public class TitleBar extends RelativeLayout {

	public static final int ID_LEFT_BUTTON = 100001;
	public static final int ID_RIGHT_BUTTON = 100002;
	public static final int ID_TITLE_TEXT = 100003;

	Button btnLeft, btnRight;
	TextView tvTitle;

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

	private void init() {

		RelativeLayout.LayoutParams layParam = new RelativeLayout.LayoutParams(
				LayoutParams.MATCH_PARENT, UIUtils.dipToPx(48));
		setLayoutParams(layParam);
		setBackgroundResource(R.drawable.bottombar);

		btnLeft = new Button(getContext());
		btnLeft.setId(ID_LEFT_BUTTON);
		RelativeLayout.LayoutParams leftParam = new RelativeLayout.LayoutParams(
				UIUtils.dipToPx(60), UIUtils.dipToPx(40));
		leftParam.leftMargin = UIUtils.dipToPx(8);
		leftParam.rightMargin = UIUtils.dipToPx(8);
		leftParam.topMargin = UIUtils.dipToPx(8);
		leftParam.bottomMargin = UIUtils.dipToPx(8);
		btnLeft.setLayoutParams(leftParam);
		btnLeft.setTextColor(Color.WHITE);
		btnLeft.setBackgroundResource(R.drawable.button_style);

		btnRight = new Button(getContext());
		btnRight.setId(ID_RIGHT_BUTTON);
		RelativeLayout.LayoutParams rightParam = new RelativeLayout.LayoutParams(
				UIUtils.dipToPx(60), UIUtils.dipToPx(40));
		rightParam.leftMargin = UIUtils.dipToPx(8);
		rightParam.rightMargin = UIUtils.dipToPx(8);
		rightParam.topMargin = UIUtils.dipToPx(8);
		rightParam.bottomMargin = UIUtils.dipToPx(8);
		rightParam.addRule(RelativeLayout.ALIGN_PARENT_RIGHT, 1);
		btnRight.setLayoutParams(rightParam);
		btnRight.setTextColor(Color.WHITE);
		btnRight.setBackgroundResource(R.drawable.button_style);

		tvTitle = new TextView(getContext());
		tvTitle.setId(ID_TITLE_TEXT);
		RelativeLayout.LayoutParams titleParam = new RelativeLayout.LayoutParams(
				LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT);
		titleParam.addRule(RelativeLayout.RIGHT_OF, ID_LEFT_BUTTON);
		titleParam.addRule(RelativeLayout.LEFT_OF, ID_RIGHT_BUTTON);
		tvTitle.setLayoutParams(titleParam);
		tvTitle.setGravity(Gravity.CENTER);
		tvTitle.setTextColor(Color.WHITE);
		tvTitle.setTextSize(18);

		addView(btnLeft);
		addView(btnRight);
		addView(tvTitle);

	}

	public void showLeftButton() {
		btnLeft.setVisibility(View.VISIBLE);
	}

	public void hideLeftButton() {
		btnLeft.setVisibility(View.INVISIBLE);
	}

	public void showRightButton() {
		btnRight.setVisibility(View.VISIBLE);
	}

	public void hideRightButton() {
		btnRight.setVisibility(View.INVISIBLE);
	}

	public void setLeftButtonEnabled(boolean enabled) {
		btnLeft.setEnabled(enabled);
	}
	
	public void setRightButtonEnabled(boolean enabled) {
		btnRight.setEnabled(enabled);
	}
	
	public void setText(int resid) {
		tvTitle.setText(resid);
	}

	public void setText(CharSequence text) {
		tvTitle.setText(text);
	}

	public void setOnLeftButtonClick(OnClickListener listener) {
		btnLeft.setOnClickListener(listener);
	}

	public void setOnRightButtonClick(OnClickListener listener) {
		btnRight.setOnClickListener(listener);
	}

	public void setLeftButtonText(int resid) {
		btnLeft.setText(resid);
	}

	public void setLeftButtonText(CharSequence text) {
		btnLeft.setText(text);
	}

	public void setRightButtonText(int resid) {
		btnRight.setText(resid);
	}

	public void setRightButtonText(CharSequence text) {
		btnRight.setText(text);
	}
}
