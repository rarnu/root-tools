package com.snda.root.hosts;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.params.CoreConnectionPNames;
import org.apache.http.protocol.HTTP;
import org.apache.http.util.EntityUtils;
import org.json.JSONObject;

import android.content.Context;
import android.content.SharedPreferences;
import android.os.Environment;
import android.os.Handler;
import android.os.Message;
import android.util.Log;

public class MiscUtils {

	public static final int MSG_DOWNLOAD_START = 0;
	public static final int MSG_DOWNLOAD_PROGRESS = 1;
	public static final int MSG_DOWNLOAD_FINISH = 2;

	public static void downloadFile(String url, String filePath, Handler h) {
		URL url2 = null;
		int filesize = 0;
		int position = 0;
		try {
			url2 = new URL(url);
			HttpURLConnection con = (HttpURLConnection) url2.openConnection();
			InputStream in = con.getInputStream();
			filesize = con.getContentLength();
			if (h != null) {
				Message msg = new Message();
				msg.what = MSG_DOWNLOAD_START;
				msg.arg1 = position;
				msg.arg2 = filesize;
				h.sendMessage(msg);
			}
			File fileOut = new File(filePath);
			FileOutputStream out = new FileOutputStream(fileOut);
			byte[] bytes = new byte[1024];
			int c;
			while ((c = in.read(bytes)) != -1) {
				out.write(bytes, 0, c);
				position += c;

				if (h != null) {
					Message msg = new Message();
					msg.what = MSG_DOWNLOAD_PROGRESS;
					msg.arg1 = position;
					msg.arg2 = filesize;
					h.sendMessage(msg);
				}
			}
			in.close();
			out.close();
			if (h != null) {
				Message msg = new Message();
				msg.what = MSG_DOWNLOAD_FINISH;
				msg.arg1 = 0;
				msg.arg2 = filesize;
				h.sendMessage(msg);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public static boolean isSdcardExists() {
		return (Environment.getExternalStorageState()
				.equals(Environment.MEDIA_MOUNTED));
	}

	public static UpdateResult hasUpdate() {
		UpdateResult ret = null;
		try {
			String retstr = CallGet(
					"http://rarnu.7thgen.info/hosts/update.php", "ver=1",
					HTTP.UTF_8);

			// {"needupdate":"<0|1>","version":"<version>","size":"<size>","text":"<text>"}

			if (!retstr.equals("")) {
				JSONObject json = new JSONObject(retstr);
				if (json.getInt("needupdate") == 1) {
					ret = new UpdateResult();
					ret.version = json.getString("version");
					ret.size = json.getString("size");
					ret.text = json.getString("text");
				}
			}
		} catch (Exception e) {

		}
		return ret;
	}

	private static String CallGet(String url, String params, String encoding)
			throws Exception {
		HttpGet request = new HttpGet(url + "?" + params);
		DefaultHttpClient hClient = new DefaultHttpClient();
		hClient.getParams().setParameter(
				CoreConnectionPNames.CONNECTION_TIMEOUT, 10000);
		HttpResponse response = hClient.execute(request);

		String result = "";
		int statusCode = response.getStatusLine().getStatusCode();
		if (statusCode == 200) {
			result = EntityUtils.toString(response.getEntity(), encoding);
		}
		return result;
	}

	public static String readAssetsFile(Context context, String fileName) {
		String ret = "";
		try {
			InputStream is = context.getAssets().open(fileName);

			int size = is.available();
			byte[] buffer = new byte[size];
			is.read(buffer);
			is.close();
			ret = new String(buffer, HTTP.UTF_8);

		} catch (IOException e) {
			Log.e("readAssetsFile", e.getMessage());
		}
		return ret;
	}

	public static String readCustomizeHosts(Context context) {
		SharedPreferences pre = context.getSharedPreferences("HtcHosts", 0);
		return pre.getString("hosts", "");
	}

	public static void writeCustomizeHosts(Context context, String hosts) {
		context.getSharedPreferences("HtcHosts", 0).edit().putString("hosts",
				hosts).commit();
	}
}
