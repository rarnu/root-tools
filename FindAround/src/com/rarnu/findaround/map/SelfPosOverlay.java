package com.rarnu.findaround.map;

import android.content.Context;
import android.location.Location;

import com.baidu.mapapi.GeoPoint;
import com.baidu.mapapi.MapView;
import com.baidu.mapapi.MyLocationOverlay;

public class SelfPosOverlay extends MyLocationOverlay {

	MapView mapView;
	boolean followLocation;

	// Bitmap marker;

	public SelfPosOverlay(Context context, MapView mapView,
			boolean followLocation) {
		super(context, mapView);
		this.mapView = mapView;
		this.followLocation = followLocation;
		// this.marker = marker;
	}

	@Override
	public void onLocationChanged(Location location) {
		super.onLocationChanged(location);
		GeoPoint point = new GeoPoint((int) (location.getLatitude() * 1e6),
				(int) (location.getLongitude() * 1e6));
		if (followLocation) {
			mapView.getController().animateTo(point);
		}
	}

	// @Override
	// protected void drawMyLocation(Canvas canvas, MapView arg1, Location arg2,
	// GeoPoint myLocation, long arg4) {
	// Point point = mapView.getProjection().toPixels(myLocation, null);
	// canvas.drawBitmap(marker, point.x, point.y - 40, null);
	// }
}
