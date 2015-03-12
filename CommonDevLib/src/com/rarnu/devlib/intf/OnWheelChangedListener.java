package com.rarnu.devlib.intf;

import com.rarnu.devlib.component.WheelView;

public interface OnWheelChangedListener {

	void onChanged(WheelView wheel, int oldValue, int newValue);
}
