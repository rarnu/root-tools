package com.rarnu.findaround.map;

import android.app.Activity;
import android.view.MotionEvent;
import android.view.View;

import com.baidu.mapapi.GeoPoint;
import com.baidu.mapapi.MapView;
import com.baidu.mapapi.RouteOverlay;
import com.baidu.mapapi.MapView.LayoutParams;
import com.rarnu.findaround.common.UIUtils;
import com.rarnu.findaround.comp.PopupView;

public class RouteOverlayEx extends RouteOverlay {

	private boolean mapMode;
	private PopupView popup;
	private MapView mapView;

	public RouteOverlayEx(Activity arg0, MapView mapView, PopupView popup) {
		super(arg0, mapView);
		this.popup = popup;
		this.mapView = mapView;
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

	public void showPopup(int i) {
		if (popup != null) {

			mapView.getController().animateTo(getItem(i).getPoint());
			setFocus(getItem(i));

			mapView.updateViewLayout(popup, new MapView.LayoutParams(
					LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT,
					getItem(i).getPoint(), MapView.LayoutParams.BOTTOM_CENTER));
			popup.setGeoPoint(getItem(i).getPoint());
			popup.setText(getItem(i).getTitle());
			popup.setVisibility(View.VISIBLE);

		}
	}

	@Override
	protected boolean onTap(int i) {

		return true;
	}

	@Override
	public boolean onTap(GeoPoint arg0, MapView arg1) {
		// if (popup != null) {
		// popup.setVisibility(View.GONE);
		// }
		return true; // super.onTap(arg0, arg1);
	}
}
