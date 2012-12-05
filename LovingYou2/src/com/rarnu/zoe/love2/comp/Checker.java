package com.rarnu.zoe.love2.comp;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import com.rarnu.zoe.love2.R;
import com.rarnu.zoe.love2.utils.UIUtils;

public class Checker extends RelativeLayout {

	public static final int STATUS_YES = 1;
	public static final int STATUS_NO = 0;

	ImageView imgStatus;
	int status = STATUS_YES;

	OnClickListener listener = null;

	private int drawableYes = 0, drawableNo = 0;

	public Checker(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public Checker(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public Checker(Context context) {
		super(context);
		init();
	}

	public void setYesDrawable(int res) {
		drawableYes = res;

	}

	public void setNoDrawable(int res) {
		drawableNo = res;
	}

	private void init() {
		setLayoutParams(new RelativeLayout.LayoutParams(UIUtils.dipToPx(52),
				UIUtils.dipToPx(52)));
		View v = inflate(getContext(), R.layout.comp_checker, null);
		v.setLayoutParams(new RelativeLayout.LayoutParams(UIUtils.dipToPx(52),
				UIUtils.dipToPx(52)));
		addView(v);

		imgStatus = (ImageView) findViewById(R.id.imgStatus);
	}

	public void setStatus(int status) {
		this.status = status;
		imgStatus.setBackgroundResource(status == STATUS_YES ? drawableYes
				: drawableNo);
	}

	public int getStatus() {
		return status;
	}

	public void setOnButtonClick(OnClickListener listener) {
		this.listener = listener;
		setOnClickListener(listener);
	}
}
