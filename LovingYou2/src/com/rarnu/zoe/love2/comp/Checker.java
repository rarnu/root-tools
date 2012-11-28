package com.rarnu.zoe.love2.comp;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import com.rarnu.zoe.love2.R;
import com.rarnu.zoe.loving.utils.UIUtils;

public class Checker extends RelativeLayout {

	public static final int STATUS_YES = 1;
	public static final int STATUS_NO = 0;

	ImageView imgStatus;
	int status = STATUS_YES;

	OnClickListener listener = null;

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

	private void init() {
		setLayoutParams(new RelativeLayout.LayoutParams(UIUtils.dipToPx(32),
				UIUtils.dipToPx(32)));
		View v = inflate(getContext(), R.layout.comp_checker, null);
		v.setLayoutParams(new RelativeLayout.LayoutParams(UIUtils.dipToPx(32),
				UIUtils.dipToPx(32)));
		addView(v);

		imgStatus = (ImageView) findViewById(R.id.imgStatus);
	}

	public void setStatus(int status) {
		this.status = status;
		imgStatus
				.setBackgroundResource(status == STATUS_YES ? R.drawable.record_yes
						: R.drawable.record_no);
	}

	public int getStatus() {
		return status;
	}

	public void setOnButtonClick(OnClickListener listener) {
		this.listener = listener;
		setOnClickListener(listener);
	}
}
