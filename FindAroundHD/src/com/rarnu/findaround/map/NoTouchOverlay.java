package com.rarnu.findaround.map;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.drawable.Drawable;
import android.os.Handler;
import android.os.Message;
import android.util.Log;
import android.view.MotionEvent;

import com.baidu.mapapi.GeoPoint;
import com.baidu.mapapi.ItemizedOverlay;
import com.baidu.mapapi.MapView;
import com.baidu.mapapi.OverlayItem;
import com.rarnu.findaround.GlobalInstance;
import com.rarnu.findaround.common.UIUtils;

public class NoTouchOverlay extends ItemizedOverlay<OverlayItem> {

	// [region] variable define
	Context context;
	boolean mapMode = false;
	Drawable marker, markerHighlight;
	Handler hJump, hShowPoiInfo;
	private List<OverlayItem> pointList = new ArrayList<OverlayItem>();

	// [/region]

	// [region] constructor
	public NoTouchOverlay(Context context, Drawable marker,
			Drawable markerHighlight, Handler hJump, Handler hShowPoiInfo) {
		super(boundCenterBottom(marker));
		this.context = context;
		this.marker = marker;
		this.markerHighlight = boundCenterBottom(markerHighlight);

		this.hJump = hJump;
		this.hShowPoiInfo = hShowPoiInfo;
		populate();
	}

	// [/region]

	// [region] business logic
	public void addOverlay(OverlayItem overlayItem) {
		pointList.add(overlayItem);
	}

	public void clearAll() {
		pointList.clear();
	}

	public void update() {
		populate();
	}

	public void setMapMode(boolean mapMode) {
		this.mapMode = mapMode;
	}

	// [/region]

	// [region] overrides
	@Override
	protected OverlayItem createItem(int arg0) {
		return pointList.get(arg0);
	}

	@Override
	public int size() {
		return pointList.size();
	}

	@Override
	public boolean onTouchEvent(MotionEvent event, MapView mapView) {
		if (event.getY() < UIUtils.dipToPx(48)
				|| event.getY() > (UIUtils.getHeight() - UIUtils.dipToPx(48))) {
			return true;
		}
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
	public boolean onTap(int i) {

		Log.e("onTap", String.valueOf(i));
		GlobalInstance.selectedInfo = GlobalInstance.listPoi.get(i).info;
		if (getLastFocusedIndex() != -1) {
			getItem(getLastFocusedIndex()).setMarker(marker);
		}
		getItem(i).setMarker(markerHighlight);
		if (hShowPoiInfo != null) {
			Message msg = new Message();
			msg.what = 1;
			msg.arg1 = i;
			hShowPoiInfo.sendMessage(msg);
		}
		return super.onTap(i);
	}

	@Override
	public boolean onTap(GeoPoint arg0, MapView arg1) {
		if (getLastFocusedIndex() != -1) {
			getItem(getLastFocusedIndex()).setMarker(marker);
		}
		if (hShowPoiInfo != null) {
			Message msg = new Message();
			msg.what = 1;
			msg.arg1 = -1;
			hShowPoiInfo.sendMessage(msg);
		}
		return super.onTap(arg0, arg1);
	}
	// [/region]
}
