package com.rarnu.findaround.map;

import java.util.ArrayList;
import java.util.List;

import android.graphics.Canvas;
import android.graphics.drawable.Drawable;
import android.view.MotionEvent;
import android.view.View;

import com.baidu.mapapi.GeoPoint;
import com.baidu.mapapi.ItemizedOverlay;
import com.baidu.mapapi.MapView;
import com.baidu.mapapi.MapView.LayoutParams;
import com.baidu.mapapi.OverlayItem;
import com.rarnu.findaround.comp.PopupView;

public class MarkOverlay extends ItemizedOverlay<OverlayItem> {

	// [region] variable define

	private List<OverlayItem> pointList = new ArrayList<OverlayItem>();
//	private Drawable marker;
	private PopupView popup;
	private MapView mvMap;
	private GeoPoint clickedPoint;

	// [/region]

	// [region] constructor
	public MarkOverlay(Drawable marker, PopupView popup, MapView mvMap) {
		super(boundCenterBottom(marker));
//		this.marker = marker;
		this.popup = popup;
		this.mvMap = mvMap;
		populate();
	}

	// [/region]

	// [region] business logic
	public void addOverlay(OverlayItem overlayItem) {
		pointList.add(overlayItem);
		showPopup();
		populate();

	}

	public void clearAll() {
		pointList.clear();
		if (popup != null) {
			popup.setVisibility(View.GONE);
		}
		populate();
	}

	public GeoPoint getClickedPoint() {
		return clickedPoint;
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
		// float x = 0;
		// float y = 0;
		// if (isFreeDraw) {
		// if (event.getAction() == MotionEvent.ACTION_DOWN) {
		// x = event.getX();
		// y = event.getY();
		// Projection projection = mapView.getProjection();
		//
		// OverlayItem overlayItem = new OverlayItem(
		// projection.fromPixels((int) x, (int) y), "", "");
		// addOverlay(overlayItem);
		//
		// }
		// }
		return super.onTouchEvent(event, mapView);
	}

	@Override
	public void draw(Canvas canvas, MapView mapView, boolean shadow) {

		// Projection projection = mapView.getProjection();
		// for (int index = size() - 1; index >= 0; index--) {
		// OverlayItem overLayItem = getItem(index);
		//
		// String title = overLayItem.getTitle();
		// Point point = projection.toPixels(overLayItem.getPoint(), null);
		//
		// Paint paintText = new Paint();
		// paintText.setColor(Color.BLUE);
		// paintText.setTextSize(15);
		// canvas.drawText(title, point.x - 30, point.y, paintText);
		// }

		super.draw(canvas, mapView, shadow);
		// boundCenterBottom(marker);
	}

	public void showPopup() {
		if (popup != null) {
			if (popup.getVisibility() != View.VISIBLE) {
				mvMap.getController().animateTo(pointList.get(0).getPoint());
				setFocus(pointList.get(0));
				clickedPoint = pointList.get(0).getPoint();
				mvMap.updateViewLayout(popup, new MapView.LayoutParams(
						LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT,
						pointList.get(0).getPoint(),
						MapView.LayoutParams.BOTTOM_CENTER));
				popup.setGeoPoint(pointList.get(0).getPoint());
				popup.setName(pointList.get(0).getTitle());
				popup.setAddress(pointList.get(0).getSnippet());
				popup.setVisibility(View.VISIBLE);
			}
		}
	}

	@Override
	protected boolean onTap(int i) {
		return true; // super.onTap(i);
	}

	@Override
	public boolean onTap(GeoPoint arg0, MapView arg1) {
		// if (popup != null) {
		// popup.setVisibility(View.GONE);
		// }
		return true; // super.onTap(arg0, arg1);
	}
	// [/region]
}
