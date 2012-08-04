package com.rarnu.findaround.map;

import android.content.Context;
import android.location.Location;

import com.baidu.mapapi.GeoPoint;
import com.baidu.mapapi.MapView;
import com.baidu.mapapi.MyLocationOverlay;

public class SelfPosOverlay extends MyLocationOverlay {

	MapView mapView;

	public SelfPosOverlay(Context context, MapView mapView) {
		super(context, mapView);
		this.mapView = mapView;
	}

	@Override
	public void onLocationChanged(Location location) {
		super.onLocationChanged(location);
		GeoPoint point = new GeoPoint((int) (location.getLatitude() * 1e6),
				(int) (location.getLongitude() * 1e6));
		mapView.getController().animateTo(point);
	}
}
