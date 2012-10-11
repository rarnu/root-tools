package com.rarnu.findaround.map;

import android.graphics.Canvas;
import android.graphics.drawable.Drawable;
import android.os.Handler;
import android.view.MotionEvent;

import com.baidu.mapapi.GeoPoint;
import com.baidu.mapapi.ItemizedOverlay;
import com.baidu.mapapi.MapView;
import com.baidu.mapapi.OverlayItem;

public class SimpleMarkOverlay extends ItemizedOverlay<OverlayItem> {

	// [region] variable define
	OverlayItem item = null;
	Handler hJump;
	boolean mapMode = false;

	// [/region]

	// [region] constructor
	public SimpleMarkOverlay(Drawable marker, OverlayItem item, Handler hJump) {
		super(boundCenterBottom(marker));
		this.item = item;
		this.hJump = hJump;
		populate();
	}

	// [/region]

	public void setMapMode(boolean mapMode) {
		this.mapMode = mapMode;
	}

	// [region] overrides
	@Override
	protected OverlayItem createItem(int arg0) {
		return item;
	}

	@Override
	public int size() {
		return 1;
	}

	@Override
	public boolean onTouchEvent(MotionEvent event, MapView mapView) {
		if (!mapMode) {
			if (event.getAction() == MotionEvent.ACTION_UP) {
				if (hJump != null) {
					hJump.sendEmptyMessage(1);
				}
			}
			return true;
		} else {
			return super.onTouchEvent(event, mapView);
		}
	}

	@Override
	public void draw(Canvas canvas, MapView mapView, boolean shadow) {

		super.draw(canvas, mapView, shadow);
		// boundCenterBottom(marker);
	}

	@Override
	protected boolean onTap(int i) {
		return false;
	}

	@Override
	public boolean onTap(GeoPoint arg0, MapView arg1) {
		return false;
	}
	// [/region]
}
