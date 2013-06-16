package com.zoe.calendar.utils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.http.protocol.HTTP;
import org.json.JSONArray;
import org.json.JSONObject;

import android.content.Context;
import android.os.Handler;
import android.os.Message;
import android.util.Log;

import com.rarnu.utils.DeviceUtilsLite;
import com.rarnu.utils.FileUtils;
import com.rarnu.utils.HttpRequest;
import com.zoe.calendar.Global;
import com.zoe.calendar.R;
import com.zoe.calendar.classes.ActivityItem;
import com.zoe.calendar.classes.CalendarItem;
import com.zoe.calendar.classes.RemoteActivityItem;
import com.zoe.calendar.classes.RemoteCalendarItem;
import com.zoe.calendar.classes.UpdateInfo;
import com.zoe.calendar.classes.WeatherInfo;
import com.zoe.calendar.common.Config;

public class APIUtils {

	private static final String BASE_URL = "http://huodongrili.com/";

	public static List<ActivityItem> downloadData(final Context context,
			final String city) {
		// set last timestamp
		List<ActivityItem> list = null;

		long timestamp = Config.getLastTimestamp(context, city);
		String url = BASE_URL + "activities";
		String param = String.format("city=%s&last_timestamp=%d", city,
				timestamp);
		try {
			String ret = HttpRequest.get(url, param, HTTP.UTF_8);
			JSONObject json = new JSONObject(ret);
			long newTimestamp = json.getLong("timestamp");
			if (city.equals("all")) {
				Global.newAllTimestamp = newTimestamp;
			} else {
				Global.newTimestamp = newTimestamp;
			}
			JSONArray jData = json.getJSONArray("data");
			if (jData != null && jData.length() != 0) {
				// load

				list = new ArrayList<ActivityItem>();
				for (int i = 0; i < jData.length(); i++) {
					RemoteActivityItem ri = new RemoteActivityItem();
					ri._id = jData.getJSONObject(i).getInt("id");
					ri.city = city;
					ri.weight = jData.getJSONObject(i).getInt("weight");
					ri.startDate = jData.getJSONObject(i).getString(
							"start_date");
					ri.startTime = jData.getJSONObject(i).getString(
							"start_time");
					ri.endDate = jData.getJSONObject(i).getString("end_date");
					ri.endTime = jData.getJSONObject(i).getString("end_time");
					ri.title = jData.getJSONObject(i).getString("title");
					ri.url = jData.getJSONObject(i).getString("url");
					ri.source = jData.getJSONObject(i).getString("source");
					ri.location = jData.getJSONObject(i).getString("location");
					ri.content = jData.getJSONObject(i).getString("content");
					String tags = "";
					for (int j = 0; j < jData.getJSONObject(i)
							.getJSONArray("tags").length(); j++) {
						tags += jData.getJSONObject(i).getJSONArray("tags")
								.getString(j)
								+ "|";
					}
					if (tags.equals("")) {
						tags = context.getString(R.string.tag_other) + "|";
					}
					ri.tags = tags;
					list.add(ActivityItem.fromRemote(ri));

				}

			}
		} catch (Exception e) {

		}

		return list;
	}

	public static List<CalendarItem> downloadCalendarItem(final Context context) {
		long timestamp = Config.getLastCalendarTimestamp(context);
		String url = BASE_URL + "calendar";
		String param = String.format("last_timestamp=%d", timestamp);

		String calendarFileName = "/data/data/" + context.getPackageName()
				+ "/calendar.json";

		try {
			String ret = HttpRequest.get(url, param, HTTP.UTF_8);
			JSONObject json = new JSONObject(ret);
			long newTimestamp = json.getLong("timestamp");
			JSONArray jData = json.getJSONArray("data");

			if (jData != null && jData.length() != 0) {
				// load
				Config.setLastCalendarTimestamp(context, newTimestamp);
				FileUtils.rewriteFile(calendarFileName, ret);
			}
		} catch (Exception e) {

		}
		try {
			return parseCalendarItem(context, calendarFileName);
		} catch (Exception e) {
			Log.e("parseCalendarItem", e.getMessage());
			return null;
		}
	}

	private static List<CalendarItem> parseCalendarItem(Context context,
			String jsonFile) throws Exception {

		if (!new File(jsonFile).exists()) {
			return null;
		}

		String jsonstr = FileUtils.readFileString(jsonFile);
		JSONObject json = new JSONObject(jsonstr);
		JSONArray jData = json.getJSONArray("data");
		List<CalendarItem> list = null;

		if (jData != null && jData.length() != 0) {
			list = new ArrayList<CalendarItem>();
			for (int i = 0; i < jData.length(); i++) {
				RemoteCalendarItem ci = new RemoteCalendarItem();
				ci.date = jData.getJSONObject(i).getString("date");
				ci.text = jData.getJSONObject(i).getString("name");
				list.add(CalendarItem.fromRemote(ci));
			}
		}
		return list;

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
		String url = BASE_URL + "app";
		String param = String.format("last_version=%d", versionCode);

		UpdateInfo info = null;

		try {
			String ret = HttpRequest.get(url, param, HTTP.UTF_8);
			JSONObject json = new JSONObject(ret);
			int hasNew = json.getInt("has_new");
			if (hasNew != 0) {
				info = new UpdateInfo();
				info.hasNew = true;
				info.url = json.getString("url");
			}
		} catch (Exception e) {

		}

		return info;
	}

	public static void feedback(final Context context, final String feedback) {

		new Thread(new Runnable() {

			@Override
			public void run() {
				// send feedback
				String device = DeviceUtilsLite.getDeviceUniqueId(context);
				curl_feedback(feedback, device);
			}
		}).start();
	}

	public static void submitNewActivity(final Context context,
			final String title, final String city, final String location,
			final String url, final String startDate, final String startTime,
			final String endDate, final String endTime, final int weight,
			final String[] tags, final String content, final String source) {

		new Thread(new Runnable() {

			@Override
			public void run() {
				String json_fmt = "title:\"%s\",city:\"%s\",location:\"%s\",url:\"%s\",start_date:\"%s\",start_time:\"%s\",end_date:\"%s\",end_time:\"%s\",weight:%d,source:\"%s\",tags:[%s],content:\"%s\"";
				String tagstr = "";
				for (String s : tags) {
					tagstr += String.format("\"%s\",", s);
				}
				if (tagstr.length() != 0 && tagstr.endsWith(",")) {
					tagstr = tagstr.substring(0, tagstr.length() - 1);
				}
				String json = String.format(json_fmt, title, city, location,
						url, startDate, startTime, endDate, endTime, weight,
						source, tagstr, content);

				// the temp method for submit a new activity
				String device = DeviceUtilsLite.getDeviceUniqueId(context);
				curl_feedback(json, device);

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
				weather.temp = weather.temp.replace("℃", "") + "℃";
				weather.weather = jWeather.getString("weather1");
				weather.wind = jWeather.getString("wind1");
				weather.index = jWeather.getString("index");
				weather.index_d = jWeather.getString("index_d");

			}
		} catch (Exception e) {
			Log.e("getWeather", e.getMessage());
		}
		return weather;
	}

	private static String curl_feedback(String feedback, String deviceId) {
		String ret = "";
		feedback = feedback.replace("\"", "\\\"").replace("/", "\\/");
		try {
			String data = String.format(
					"{\"data\":\"%s\",\"device_id\":\"%s\"}", feedback,
					deviceId);
			ret = HttpRequest.simplePost(BASE_URL + "feedback/", data,
					HTTP.UTF_8);
			Log.e("curl_feedback", ret);
		} catch (Exception e) {
			Log.e("curl_feedback", e.getMessage());
		}
		return ret;
	}

}
