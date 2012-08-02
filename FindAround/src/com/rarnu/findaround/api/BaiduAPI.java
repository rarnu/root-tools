package com.rarnu.findaround.api;

import java.util.ArrayList;
import java.util.List;

import org.apache.http.protocol.HTTP;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.util.Log;

import com.baidu.mapapi.GeoPoint;
import com.baidu.mapapi.MKPoiInfo;
import com.rarnu.findaround.GlobalInstance;
import com.rarnu.findaround.MainApplication;

public class BaiduAPI {

	private static final String API_GEO = "http://api.map.baidu.com/geocoder";
	private static final String API_PLACE = "http://api.map.baidu.com/place/search";

	public static String getAddressViaGeo(double latitude, double longitude) {

		String reqFmt = "output=json&location=%f,%f&key=%s";
		String reqParam = String.format(reqFmt, latitude, longitude,
				MainApplication.webKey);

		String ret = HttpRequest.get(API_GEO, reqParam, HTTP.UTF_8);

		try {
			JSONObject json = new JSONObject(ret);
			String addr = "";
			if (json.getString("status").equals("OK")) {
				JSONObject resultNode = json.getJSONObject("result");
				addr = resultNode.getString("formatted_address");
				JSONObject cityNode = resultNode
						.getJSONObject("addressComponent");
				GlobalInstance.city = cityNode.getString("city");
			}

			return addr;

		} catch (JSONException e) {
			Log.e("getAddressViaGeo", e.getMessage());
			return "";
		}

	}

	public static List<MKPoiInfo> getPoiListViaGeo(double latitude,
			double longitude, int radius, String keyword) {
		String reqFmt = "query=%s&location=%f,%f&radius=%d&output=json&key=%s";
		String reqParam = String.format(reqFmt, keyword, latitude, longitude,
				radius, MainApplication.webKey);
		String ret = HttpRequest.get(API_PLACE, reqParam, HTTP.UTF_8);
		List<MKPoiInfo> result = null;
		try {
			JSONObject json = new JSONObject(ret);
			int lat, lng;
			if (json.getString("status").equals("OK")) {
				result = new ArrayList<MKPoiInfo>();
				JSONArray array = json.getJSONArray("results");
				for (int i = 0; i < array.length(); i++) {
					MKPoiInfo info = new MKPoiInfo();
					info.name = array.getJSONObject(i).getString("name");
					info.address = array.getJSONObject(i).getString("address");
					lat = (int) (array.getJSONObject(i)
							.getJSONObject("location").getDouble("lat") * 1e6);
					lng = (int) (array.getJSONObject(i)
							.getJSONObject("location").getDouble("lng") * 1e6);
					info.pt = new GeoPoint(lat, lng);
					result.add(info);
				}
			}
		} catch (Exception e) {

		}
		return result;
	}

}
