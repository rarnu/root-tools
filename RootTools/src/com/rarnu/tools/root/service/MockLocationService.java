package com.rarnu.tools.root.service;

import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.location.Location;
import android.location.LocationManager;
import android.os.IBinder;

public class MockLocationService extends Service {

	public static MockLocationService _instance;

	private double lat;
	private double lng;
	private double altitude;
	private float bearing;
	private float speed;
	private float accuracy;
	public static boolean forFlag = true;
	private boolean finishFlag = false;

	private LocationManager mLocationManager;

	@Override
	public void onStart(Intent intent, int startId) {
		_instance = this;
		try {
			Thread.sleep(1500);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}

		forFlag = true;
		mLocationManager = (LocationManager) getSystemService(Context.LOCATION_SERVICE);

		// lat = intent.getDoubleExtra(Constants.EXTRA_LAT, 0);
		// lng = intent.getDoubleExtra(Constants.EXTRA_LNG, 0);
		// altitude = intent.getDoubleExtra(Constants.EXTRA_ALTITUDE, 0);
		// bearing = intent.getFloatExtra(Constants.EXTRA_BEARING, 0f);
		// speed = intent.getFloatExtra(Constants.EXTRA_SPEED, 0f);
		// accuracy = intent.getFloatExtra(Constants.EXTRA_ACCURACY, 0f);
		finishFlag = intent.getBooleanExtra("finishFlag", false);

		if (finishFlag) {
			MockLocationService.this.stopSelf();
			return;
		}

		new Thread(new InitThread()).start();
	}

	@Override
	public void onDestroy() {
		_instance = null;
		super.onDestroy();
	}

	public class InitThread implements Runnable {
		public void run() {
			try {
				while (forFlag) {
					try {

						Thread.sleep(1000);

					} catch (InterruptedException e) {

					}

					// Set one position
					Location location = new Location(
							LocationManager.GPS_PROVIDER);
					location.setLatitude(lat);
					location.setLongitude(lng);
					location.setAltitude(altitude);
					location.setBearing(bearing);
					location.setSpeed(speed);
					location.setAccuracy(accuracy);

					// set the time in the location. If the time on this
					// location
					// matches the time on the one in the previous set call, it
					// will be
					// ignored
					location.setTime(System.currentTimeMillis());

					mLocationManager.setTestProviderLocation(
							LocationManager.GPS_PROVIDER, location);
				}

			} catch (Exception e) {

			}
		}

	}

	@Override
	public IBinder onBind(Intent intent) {
		return null;
	}

}
