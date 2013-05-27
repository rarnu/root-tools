package com.zoe.calendar.utils;

import java.util.List;

import org.apache.http.protocol.HTTP;
import org.json.JSONObject;

import android.content.Context;
import android.os.Handler;
import android.os.Message;

import com.rarnu.utils.DeviceUtilsLite;
import com.rarnu.utils.HttpRequest;
import com.zoe.calendar.classes.ActivityItem;
import com.zoe.calendar.classes.UpdateInfo;
import com.zoe.calendar.classes.WeatherInfo;

public class APIUtils {

	public static List<ActivityItem> downloadData(final int timestamp) {
		// TODO: download
		// set last timestamp
		return null;
	}

	public interface UpdateCallback {
		void onUpdateFound(UpdateInfo update);
	}

	public static void checkUpdate(Context context,
			final UpdateCallback callback) {

		final int versionCode = DeviceUtilsLite.getAppVersionCode(context);

		final Handler hUpdateCallback = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					if (callback != null) {
						callback.onUpdateFound((UpdateInfo) msg.obj);
					}
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				UpdateInfo info = getUpdateInfo(versionCode);
				Message msg = new Message();
				msg.what = 1;
				msg.obj = info;
				hUpdateCallback.sendMessage(msg);

			}
		}).start();

	}

	private static UpdateInfo getUpdateInfo(int versionCode) {
		// TODO: check update
		return null;
	}

	public static void feedback(final String feedback) {
		
		new Thread(new Runnable() {
			
			@Override
			public void run() {
				// TODO: send feedback
				
			}
		}).start();
	}

	public interface WeatherCallback {
		void onGetWeather(WeatherInfo weather);
	}

	public static void getWeather(final int cityId,
			final WeatherCallback callback) {

		final Handler hCallback = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					if (callback != null) {
						callback.onGetWeather((WeatherInfo) msg.obj);
					}
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {
			@Override
			public void run() {
				WeatherInfo info = getWeather(cityId);
				Message msg = new Message();
				msg.what = 1;
				msg.obj = info;
				hCallback.sendMessage(msg);
			}
		}).start();

	}

	private static WeatherInfo getWeather(int cityId) {

		WeatherInfo weather = null;
		String url = String.format("http://m.weather.com.cn/data/%d.html",
				cityId);
		try {
			String ret = HttpRequest.get(url, "", HTTP.UTF_8);
			JSONObject json = new JSONObject(ret);
			JSONObject jWeather = json.getJSONObject("weatherinfo");

			if (jWeather != null) {
				weather = new WeatherInfo();
				weather.temp = jWeather.getString("temp1");
				weather.weather = jWeather.getString("weather1");
				weather.wind = jWeather.getString("wind1");
				weather.image_title = jWeather.getString("image_title1");
				weather.index = jWeather.getString("index");
				weather.index_d = jWeather.getString("index_d");

			}
		} catch (Exception e) {

		}
		return weather;
	}

}
