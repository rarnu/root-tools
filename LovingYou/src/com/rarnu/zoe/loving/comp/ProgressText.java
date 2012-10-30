package com.rarnu.zoe.loving.comp;

import android.content.Context;
import android.graphics.Color;
import android.util.AttributeSet;
import android.view.Gravity;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.zoe.loving.R;
import com.rarnu.zoe.loving.utils.UIUtils;

public class ProgressText extends RelativeLayout {

	private TextView tvMessage;

	public ProgressText(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public ProgressText(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public ProgressText(Context context) {
		super(context);
		init();
	}

	private void init() {

		setBackgroundResource(R.drawable.bottombar);
		RelativeLayout layInner = new RelativeLayout(getContext());
		RelativeLayout.LayoutParams layInnerParam = new RelativeLayout.LayoutParams(
				LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT);
		layInnerParam.leftMargin = UIUtils.dipToPx(4);
		layInnerParam.rightMargin = UIUtils.dipToPx(4);
		layInnerParam.topMargin = UIUtils.dipToPx(4);
		layInnerParam.bottomMargin = UIUtils.dipToPx(4);
		layInner.setBackgroundColor(Color.WHITE);
		layInner.setLayoutParams(layInnerParam);

		addView(layInner);

		ProgressBar pb = new ProgressBar(getContext());
		RelativeLayout.LayoutParams pbParam = new RelativeLayout.LayoutParams(
				UIUtils.dipToPx(24), UIUtils.dipToPx(24));
		pbParam.addRule(RelativeLayout.CENTER_VERTICAL, 1);
		pbParam.leftMargin = UIUtils.dipToPx(8);
		pb.setLayoutParams(pbParam);
		layInner.addView(pb);

		tvMessage = new TextView(getContext());
		RelativeLayout.LayoutParams tvParam = new RelativeLayout.LayoutParams(
				LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
		tvParam.addRule(RelativeLayout.CENTER_IN_PARENT, 1);
		tvMessage.setLayoutParams(tvParam);
		tvMessage.setGravity(Gravity.LEFT | Gravity.CENTER_VERTICAL);
		tvMessage.setTextColor(Color.BLACK);
		
		layInner.addView(tvMessage);
		tvMessage.setText("Loading...");
	}

	public void setText(CharSequence text) {
		tvMessage.setText(text);
	}
	
	public void setText(int resid) {
		tvMessage.setText(resid);
	}
}
