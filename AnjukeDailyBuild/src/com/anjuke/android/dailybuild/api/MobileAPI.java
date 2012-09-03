package com.anjuke.android.dailybuild.api;

import java.util.ArrayList;
import java.util.List;

import org.apache.http.protocol.HTTP;
import org.json.JSONArray;
import org.json.JSONObject;

import com.anjuke.android.dailybuild.HttpRequest;

public class MobileAPI {

	private static final String HOST = "http://rarnu.7thgen.info/anjuke/dailybuild/list.php";

	public static List<String> getProjectList() {
		String ret = HttpRequest.get(HOST, "", HTTP.UTF_8);
		try {
			JSONObject json = new JSONObject(ret);
			List<String> result = new ArrayList<String>();
			JSONArray array = json.getJSONArray("data");
			for (int i=0; i<array.length(); i++) {
				result.add(array.getString(i));
			}
			return result;
		} catch (Exception e) {
			return null;
		}
	}
}
