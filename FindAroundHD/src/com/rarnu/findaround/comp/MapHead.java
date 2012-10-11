package com.rarnu.findaround.comp;

import com.rarnu.findaround.R;
import com.rarnu.findaround.common.UIUtils;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.ImageView;
import android.widget.RelativeLayout;

public class MapHead extends RelativeLayout {

	ImageView ivCenter;

	public MapHead(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public MapHead(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public MapHead(Context context) {
		super(context);
		init();
	}

	private void init() {
		ivCenter = new ImageView(getContext());
		ivCenter.setBackgroundResource(R.drawable.marker);
		RelativeLayout.LayoutParams rlp = new LayoutParams(UIUtils.dipToPx(14),
				UIUtils.dipToPx(18));
		rlp.addRule(RelativeLayout.CENTER_IN_PARENT, 1);
		ivCenter.setLayoutParams(rlp);
		addView(ivCenter);
		ivCenter.setVisibility(View.GONE);

	}

	public void setCenterVisible(boolean visible) {
		ivCenter.setVisibility(visible ? View.VISIBLE : View.GONE);
	}

}
