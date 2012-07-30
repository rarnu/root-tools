package com.rarnu.findaround.api;

import org.apache.http.protocol.HTTP;
import org.json.JSONException;
import org.json.JSONObject;

import android.util.Log;

import com.rarnu.findaround.GlobalInstance;
import com.rarnu.findaround.MainApplication;

public class BaiduAPI {

	private static final String API_GEO = "http://api.map.baidu.com/geocoder";

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
				JSONObject cityNode = resultNode.getJSONObject("addressComponent");
				GlobalInstance.city = cityNode.getString("city");
			}

			return addr;

		} catch (JSONException e) {
			Log.e("getAddressViaGeo", e.getMessage());
			return "";
		}

	}
}
