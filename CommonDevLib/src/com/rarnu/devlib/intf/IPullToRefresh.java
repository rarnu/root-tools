package com.rarnu.devlib.intf;

import android.view.View;
import android.view.animation.Interpolator;
import com.rarnu.devlib.base.PullToRefreshBase;

public interface IPullToRefresh<T extends View> {

	boolean demo();

	PullToRefreshBase.Mode getCurrentMode();

	boolean getFilterTouchEvents();

	ILoadingLayout getLoadingLayoutProxy();

	ILoadingLayout getLoadingLayoutProxy(boolean includeStart, boolean includeEnd);

	PullToRefreshBase.Mode getMode();

	T getRefreshableView();

	boolean getShowViewWhileRefreshing();

	PullToRefreshBase.State getState();

	boolean isPullToRefreshEnabled();

	boolean isPullToRefreshOverScrollEnabled();

	boolean isRefreshing();

	boolean isScrollingWhileRefreshingEnabled();

	void onRefreshComplete();

	void setFilterTouchEvents(boolean filterEvents);

	void setMode(PullToRefreshBase.Mode mode);

	void setOnPullEventListener(PullToRefreshBase.OnPullEventListener<T> listener);

	void setOnRefreshListener(PullToRefreshBase.OnRefreshListener<T> listener);

	void setOnRefreshListener(PullToRefreshBase.OnRefreshListener2<T> listener);

	void setPullToRefreshOverScrollEnabled(boolean enabled);

	void setRefreshing();

	void setRefreshing(boolean doScroll);

	void setScrollAnimationInterpolator(Interpolator interpolator);

	void setScrollingWhileRefreshingEnabled(boolean scrollingWhileRefreshingEnabled);

	void setShowViewWhileRefreshing(boolean showView);

}