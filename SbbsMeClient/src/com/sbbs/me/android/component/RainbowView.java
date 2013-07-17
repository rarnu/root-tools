package com.sbbs.me.android.component;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.RelativeLayout;

import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.R;

public class RainbowView extends RelativeLayout {

	private RelativeLayout[] blocks;
	int columnCount = 0;
	int rowCount = 0;

	public RainbowView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public RainbowView(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public RainbowView(Context context) {
		super(context);
		init();
	}

	private void init() {
		setPadding(0, 0, 0, UIUtils.dipToPx(4));
	}

	/**
	 * 
	 * @param count
	 * @param mode
	 *            0:from left, 1:from right
	 */
	public void setBlockCount(int count, int mode) {

		if (count < 7) {
			rowCount = count;
			columnCount = 1;
		} else {
			rowCount = 7;
			columnCount = count / 7 + (count % 7 == 0 ? 0 : 1);
		}
		blocks = new RelativeLayout[count];
		int viewId = 200000;
		for (int i = 0; i < count; i++) {
			blocks[i] = new RelativeLayout(getContext());
			blocks[i].setId(viewId);
			RelativeLayout.LayoutParams blp = new RelativeLayout.LayoutParams(
					UIUtils.dipToPx(8), UIUtils.dipToPx(8));
			if (i % 7 == 0) {
				blp.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM,
						RelativeLayout.TRUE);
			} else {
				blp.addRule(RelativeLayout.ABOVE, viewId - 1);
			}
			if (i >= 7) {
				blp.addRule(mode == 0 ? RelativeLayout.RIGHT_OF
						: RelativeLayout.LEFT_OF, viewId - 7);
			} else {
				if (mode == 1) {
					blp.addRule(RelativeLayout.ALIGN_PARENT_RIGHT, RelativeLayout.TRUE);
				}
			}
			blp.rightMargin = 1;
			blp.topMargin = 1;
			blocks[i].setLayoutParams(blp);
			blocks[i].setBackgroundColor(getResources().getColor(
					R.color.google_light_green));
			addView(blocks[i]);
			viewId++;

		}
		RelativeLayout.LayoutParams rllp = (RelativeLayout.LayoutParams) getLayoutParams();
		if (rllp == null) {
			rllp = new RelativeLayout.LayoutParams(-1, -1);
		}
		rllp.width = UIUtils.dipToPx(8) * columnCount;
		rllp.height = UIUtils.dipToPx(8) * rowCount + UIUtils.dipToPx(4);
		setLayoutParams(rllp);
	}
}
