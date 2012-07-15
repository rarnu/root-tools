package com.rarnu.tools.root.comp;

import java.util.ArrayList;
import java.util.List;

import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Point;
import android.graphics.drawable.Drawable;

import com.baidu.mapapi.GeoPoint;
import com.baidu.mapapi.ItemizedOverlay;
import com.baidu.mapapi.MapView;
import com.baidu.mapapi.OverlayItem;
import com.baidu.mapapi.Projection;

public class MarkOverlay extends ItemizedOverlay<OverlayItem> {

	// [region] variable define
	private List<OverlayItem> pointList = new ArrayList<OverlayItem>();
	private Drawable marker;
	private MapView mvMap;
	private GeoPoint clickedPoint;

	// [/region]

	// [region] constructor
	public MarkOverlay(Drawable marker, MapView mvMap) {
		super(boundCenterBottom(marker));
		this.marker = marker;
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

		mvMap.getController().animateTo(pointList.get(i).getPoint());
		setFocus(pointList.get(i));
		clickedPoint = pointList.get(i).getPoint();

		return true;
	}

	@Override
	public boolean onTap(GeoPoint arg0, MapView arg1) {
		return super.onTap(arg0, arg1);
	}
	// [/region]
}
