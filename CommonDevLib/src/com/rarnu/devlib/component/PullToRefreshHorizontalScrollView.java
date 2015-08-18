package com.rarnu.devlib.component;

import android.content.Context;
import android.os.Build.VERSION;
import android.os.Build.VERSION_CODES;
import android.util.AttributeSet;
import android.view.View;
import android.widget.HorizontalScrollView;
import com.rarnu.devlib.R;
import com.rarnu.devlib.base.PullToRefreshBase;
import com.rarnu.devlib.component.tools.OverscrollHelper;

public class PullToRefreshHorizontalScrollView extends PullToRefreshBase<HorizontalScrollView> {

	public PullToRefreshHorizontalScrollView(Context context) {
		super(context);
	}

	public PullToRefreshHorizontalScrollView(Context context, AttributeSet attrs) {
		super(context, attrs);
	}

	public PullToRefreshHorizontalScrollView(Context context, Mode mode) {
		super(context, mode);
	}

	public PullToRefreshHorizontalScrollView(Context context, Mode mode, AnimationStyle style) {
		super(context, mode, style);
	}

	@Override
	public final Orientation getPullToRefreshScrollDirection() {
		return Orientation.HORIZONTAL;
	}

	@Override
	protected HorizontalScrollView createRefreshableView(Context context, AttributeSet attrs) {
		HorizontalScrollView scrollView;

		if (VERSION.SDK_INT >= VERSION_CODES.GINGERBREAD) {
			scrollView = new InternalHorizontalScrollViewSDK9(context, attrs);
		} else {
			scrollView = new HorizontalScrollView(context, attrs);
		}

		scrollView.setId(R.id.scrollview);
		return scrollView;
	}

	@Override
	protected boolean isReadyForPullStart() {
		return mRefreshableView.getScrollX() == 0;
	}

	@Override
	protected boolean isReadyForPullEnd() {
		View scrollViewChild = mRefreshableView.getChildAt(0);
		if (null != scrollViewChild) {
			return mRefreshableView.getScrollX() >= (scrollViewChild.getWidth() - getWidth());
		}
		return false;
	}

	final class InternalHorizontalScrollViewSDK9 extends HorizontalScrollView {

		public InternalHorizontalScrollViewSDK9(Context context, AttributeSet attrs) {
			super(context, attrs);
		}

		@Override
		protected boolean overScrollBy(int deltaX, int deltaY, int scrollX, int scrollY, int scrollRangeX,
				int scrollRangeY, int maxOverScrollX, int maxOverScrollY, boolean isTouchEvent) {

			final boolean returnValue = super.overScrollBy(deltaX, deltaY, scrollX, scrollY, scrollRangeX,
					scrollRangeY, maxOverScrollX, maxOverScrollY, isTouchEvent);

			OverscrollHelper.overScrollBy(PullToRefreshHorizontalScrollView.this, deltaX, scrollX, deltaY, scrollY,
					getScrollRange(), isTouchEvent);

			return returnValue;
		}

		private int getScrollRange() {
			int scrollRange = 0;
			if (getChildCount() > 0) {
				View child = getChildAt(0);
				scrollRange = Math.max(0, child.getWidth() - (getWidth() - getPaddingLeft() - getPaddingRight()));
			}
			return scrollRange;
		}
	}
}
