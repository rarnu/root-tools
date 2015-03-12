package com.rarnu.devlib.intf;

import com.rarnu.devlib.component.WheelView;

public interface OnWheelScrollListener {

	void onScrollingStarted(WheelView wheel);

	void onScrollingFinished(WheelView wheel);
}
