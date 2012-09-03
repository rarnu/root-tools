package com.anjuke.android.dailybuild.api;

import java.util.ArrayList;
import java.util.List;

import org.apache.http.protocol.HTTP;
import org.json.JSONArray;
import org.json.JSONObject;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;

import com.anjuke.android.dailybuild.HttpRequest;

public class MobileAPI {

	private static final String HOST = "http://rarnu.7thgen.info/anjuke/dailybuild/";
	private static final String GET_LIST = HOST + "list.php";
	private static final String GET_FILE_LIST = HOST + "list_files.php";

	public static List<String> getProjectList() {
		String ret = HttpRequest.get(GET_LIST, "", HTTP.UTF_8);
		try {
			JSONObject json = new JSONObject(ret);
			List<String> result = new ArrayList<String>();
			JSONArray array = json.getJSONArray("data");
			for (int i = 0; i < array.length(); i++) {
				result.add(array.getString(i));
			}
			return result;
		} catch (Exception e) {
			return null;
		}
	}

	public static List<String> getFileList(String folder, int count) {
		String ret = HttpRequest.get(GET_FILE_LIST,
				String.format("folder=%s&count=%d", folder.toLowerCase(), count), HTTP.UTF_8);
		
		if (ret == null || ret.trim().equals("")) {
			return null;
		} else {
			String[] files = ret.split("\\|\\|");
			List<String> result = new ArrayList<String>();
			for (String s: files) {
				result.add(s);
			}
			return result;
		}
	}
	
	public static void downloadFile(Context context, String folder, String fileName) {
		String url = HOST + folder.toLowerCase() + "/" + fileName;
		Intent inDownload = new Intent(Intent.ACTION_VIEW);
		inDownload.setData(Uri.parse(url));
		context.startActivity(inDownload);
	}
}
