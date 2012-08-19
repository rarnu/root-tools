package com.rarnu.findaround.map;

import android.app.Activity;
import android.view.MotionEvent;

import com.baidu.mapapi.MapView;
import com.baidu.mapapi.RouteOverlay;
import com.rarnu.findaround.common.UIUtils;

public class RouteOverlayEx extends RouteOverlay {

	private boolean mapMode;
	
	public RouteOverlayEx(Activity arg0, MapView arg1) {
		super(arg0, arg1);
	}
	
	@Override
	public boolean onTouchEvent(MotionEvent event, MapView mapView) {
		if (!mapMode) {
			return true;
		}
		if (event.getY() < UIUtils.dipToPx(48)
				|| event.getY() > (UIUtils.getHeight() - UIUtils.dipToPx(48))) {
			return true;
		}
		return super.onTouchEvent(event, mapView);
	}

	public boolean isMapMode() {
		return mapMode;
	}

	public void setMapMode(boolean mapMode) {
		this.mapMode = mapMode;
	}

}
