package com.anjuke.android.dailybuild.api;

import java.util.ArrayList;
import java.util.List;

import org.apache.http.protocol.HTTP;
import org.json.JSONArray;
import org.json.JSONObject;

import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.net.Uri;
import android.util.Log;

import com.anjuke.android.dailybuild.HttpRequest;

public class MobileAPI {

	private static final String HOST = "http://rarnu.7thgen.info/anjuke/dailybuild/";
	private static final String GET_LIST = HOST + "list.php";
	private static final String GET_FILE_LIST = HOST + "list_files.php";
	private static final String CHECK_UPDATE = HOST + "check_update.php";

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
		String ret = HttpRequest.get(GET_FILE_LIST, String.format(
				"folder=%s&count=%d", folder.toLowerCase(), count), HTTP.UTF_8);

		if (ret == null || ret.trim().equals("")) {
			return null;
		} else {
			String[] files = ret.split("\\|\\|");
			List<String> result = new ArrayList<String>();
			for (String s : files) {
				result.add(s);
			}
			return result;
		}
	}

	public static void downloadFile(Context context, String folder,
			String fileName) {
		String url = HOST + folder.toLowerCase() + "/" + fileName;
		Intent inDownload = new Intent(Intent.ACTION_VIEW);
		inDownload.setData(Uri.parse(url));
		context.startActivity(inDownload);
	}

	public static UpdateInfo checkUpdate(int version) {
		UpdateInfo result = null;
		try {
			String ret = HttpRequest.get(CHECK_UPDATE,
					String.format("version=%d", version), HTTP.UTF_8);
			JSONObject json = new JSONObject(ret);
			result = new UpdateInfo();
			result.result = json.getInt("result");
			result.versionCode = json.getInt("version_code");
			result.versionName = json.getString("version_name");
			result.file = json.getString("file");
			result.size = json.getString("size");
		} catch (Exception e) {
			Log.e("checkUpdate", e.getMessage());
			result = null;
		}
		return result;
	}

	public static int getAppVersionCode(Context context) {
		int versionCode = 0;
		try {

			PackageInfo pi = context.getPackageManager().getPackageInfo(
					context.getPackageName(), 0);
			versionCode = pi.versionCode;

		} catch (Exception e) {

		}
		return versionCode;
	}
}
