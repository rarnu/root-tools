package com.rarnu.findaround;

import java.util.ArrayList;
import java.util.List;

import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Point;
import android.graphics.drawable.Drawable;
import android.view.MotionEvent;
import android.view.View;

import com.baidu.mapapi.GeoPoint;
import com.baidu.mapapi.ItemizedOverlay;
import com.baidu.mapapi.MapView;
import com.baidu.mapapi.OverlayItem;
import com.baidu.mapapi.Projection;
import com.baidu.mapapi.MapView.LayoutParams;

public class MarkOverlay extends ItemizedOverlay<OverlayItem> {

	// [region] variable define
	private boolean isFreeDraw = false;
	private List<OverlayItem> pointList = new ArrayList<OverlayItem>();
	private Drawable marker;
	private PopupView popup;
	private MapView mvMap;
	private GeoPoint clickedPoint;

	// [/region]

	// [region] constructor
	public MarkOverlay(Drawable marker, PopupView popup, MapView mvMap) {
		super(boundCenterBottom(marker));
		this.marker = marker;
		this.popup = popup;
		this.mvMap = mvMap;
		populate();
	}

	// [/region]

	// [region] business logic
	public void addOverlay(OverlayItem overlayItem) {
		pointList.add(overlayItem);
		populate();
	}

	public void clearAll() {
		pointList.clear();
		popup.setVisibility(View.GONE);
		populate();
	}

	public void setFreeDraw(boolean freeDraw) {
		isFreeDraw = freeDraw;
	}

	public boolean getFreeDraw() {
		return isFreeDraw;
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
		float x = 0;
		float y = 0;
		if (isFreeDraw) {
			if (event.getAction() == MotionEvent.ACTION_DOWN) {
				x = event.getX();
				y = event.getY();
				Projection projection = mapView.getProjection();

				OverlayItem overlayItem = new OverlayItem(projection.fromPixels((int) x, (int) y), "", "");
				addOverlay(overlayItem);

			}
		}
		return super.onTouchEvent(event, mapView);
	}

	@Override
	public void draw(Canvas canvas, MapView mapView, boolean shadow) {

		Projection projection = mapView.getProjection();
		for (int index = size() - 1; index >= 0; index--) {
			OverlayItem overLayItem = getItem(index);

			String title = overLayItem.getTitle();
			Point point = projection.toPixels(overLayItem.getPoint(), null);

			Paint paintText = new Paint();
			paintText.setColor(Color.BLUE);
			paintText.setTextSize(15);
			canvas.drawText(title, point.x - 30, point.y, paintText);
		}

		super.draw(canvas, mapView, shadow);
		boundCenterBottom(marker);
	}

	@Override
	protected boolean onTap(int i) {
		if (!isFreeDraw) {
			mvMap.getController().animateTo(pointList.get(i).getPoint());
			setFocus(pointList.get(i));
			clickedPoint = pointList.get(i).getPoint();
			mvMap.updateViewLayout(popup, new MapView.LayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT,
					pointList.get(i).getPoint(), MapView.LayoutParams.BOTTOM_CENTER));
			popup.setGeoPoint(pointList.get(i).getPoint());
			popup.setName(pointList.get(i).getTitle());
			popup.setAddress(pointList.get(i).getSnippet());
			popup.setVisibility(View.VISIBLE);
		}
		return true;
	}

	@Override
	public boolean onTap(GeoPoint arg0, MapView arg1) {
		popup.setVisibility(View.GONE);
		return super.onTap(arg0, arg1);
	}
	// [/region]
}
