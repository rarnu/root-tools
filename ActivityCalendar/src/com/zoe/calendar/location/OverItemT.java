package com.zoe.calendar.location;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.graphics.drawable.Drawable;

import com.baidu.mapapi.map.ItemizedOverlay;
import com.baidu.mapapi.map.MapView;
import com.baidu.mapapi.map.OverlayItem;
import com.baidu.platform.comapi.basestruct.GeoPoint;

public class OverItemT extends ItemizedOverlay<OverlayItem> {

	private List<OverlayItem> mGeoList = new ArrayList<OverlayItem>();

	public OverItemT(Drawable marker, Context context, GeoPoint pt,
			String title, MapView mMapView) {
		super(marker, mMapView);
		OverlayItem item = new OverlayItem(pt, title, null);
		mGeoList.add(item);
		addItem(item);
	}
}
