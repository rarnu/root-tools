package com.rarnu.devlib.component;

import android.content.Context;
import android.graphics.Color;
import android.graphics.Rect;
import android.util.AttributeSet;
import android.view.View;
import android.view.animation.ScaleAnimation;
import android.widget.RelativeLayout;

import com.rarnu.utils.AnimateUtils;

/**
 * 
 * must use in MergeView
 * 
 */
public class BlockView extends RelativeLayout {

	public int id;
	// will merge grids?
	public int xsize = 1;
	public int ysize = 1;
	// focus index, must be filled
	public int index = -1;
	// relative
	public int below = -1;
	public int toRightOf = -1;

	private float scaleFactor = 1.05F;
	private int intervalFactor = 150;
	private View v = null;
	private int focusColor = Color.WHITE;

	ScaleAnimation saBig, saSmall;

	public interface FocusCallback {
		void onFocusChanged(boolean focused, int index);
	}

	FocusCallback callback = null;

	public BlockView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public BlockView(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public BlockView(Context context) {
		super(context);
		init();
	}

	public void setFocusCallback(FocusCallback callback) {
		this.callback = callback;
	}

	private void init() {
		setFocusable(true);
		setFocusableInTouchMode(true);
		setClickable(true);

		saBig = AnimateUtils.getScaleAnimation(1.0F, scaleFactor,
				intervalFactor, true);
		saSmall = AnimateUtils.getScaleAnimation(scaleFactor, 1.0F,
				intervalFactor, true);

	}

	public void loadLayout(int res) {
		v = inflate(getContext(), res, null);
		addView(v, new RelativeLayout.LayoutParams(
				RelativeLayout.LayoutParams.MATCH_PARENT,
				RelativeLayout.LayoutParams.MATCH_PARENT));
	}

	public View getInnerView() {
		return v;
	}

	@Override
	protected void onFocusChanged(boolean gainFocus, int direction,
			Rect previouslyFocusedRect) {
		super.onFocusChanged(gainFocus, direction, previouslyFocusedRect);

		if (callback != null) {
			callback.onFocusChanged(gainFocus, index);
		}
		if (gainFocus) {
			bringToFront();
			setBackgroundColor(focusColor);
			startAnimation(saBig);
		} else {
			setBackgroundColor(0x00000000);
			startAnimation(saSmall);
		}

	}

}