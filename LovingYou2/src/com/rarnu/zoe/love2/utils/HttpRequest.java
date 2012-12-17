package com.rarnu.zoe.love2.utils;

import java.io.File;
import java.nio.charset.Charset;
import java.util.List;

import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.mime.content.FileBody;
import org.apache.http.entity.mime.content.StringBody;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.protocol.HTTP;
import org.apache.http.util.EntityUtils;
import org.apache.http.entity.mime.MultipartEntity;

import android.util.Log;

public class HttpRequest {

	public static String post(String host, List<BasicNameValuePair> params,
			String encoding) {

		HttpPost httpPost = new HttpPost(host);
		try {

			BasicHttpParams httpParams = new BasicHttpParams();
			HttpConnectionParams.setConnectionTimeout(httpParams, 5000);
			HttpConnectionParams.setSoTimeout(httpParams, 5000);

			DefaultHttpClient client = new DefaultHttpClient(httpParams);

			UrlEncodedFormEntity p_entity = new UrlEncodedFormEntity(params,
					encoding);
			httpPost.setEntity(p_entity);
			HttpResponse response = client.execute(httpPost);

			String result = "";
			int statusCode = response.getStatusLine().getStatusCode();

			if (statusCode == 200) {
				result = EntityUtils.toString(response.getEntity(), encoding);
			}

			return result;

		} catch (Exception e) {

			return "";
		}
	}

	public static String get(String host, String params, String encoding) {

		HttpGet request = new HttpGet(host + "?" + params);
		// Log.e("get", host + "?" + params);
		try {
			BasicHttpParams httpParams = new BasicHttpParams();
			HttpConnectionParams.setConnectionTimeout(httpParams, 5000);
			HttpConnectionParams.setSoTimeout(httpParams, 5000);

			DefaultHttpClient client = new DefaultHttpClient(httpParams);
			HttpResponse response = client.execute(request);

			String result = "";
			int statusCode = response.getStatusLine().getStatusCode();
			if (statusCode == 200) {
				result = EntityUtils.toString(response.getEntity(), encoding);
			}
			return result;
		} catch (Exception e) {
			return "";
		}
	}

	public static String postFile(String host, List<BasicNameValuePair> params,
			String fileName, String encoding) {
		HttpPost post = new HttpPost(host);
		try {
			File file = new File(fileName);
			MultipartEntity multipart = new MultipartEntity();
			for (BasicNameValuePair pair : params) {
				multipart.addPart(
						pair.getName(),
						new StringBody(pair.getValue(), Charset
								.forName(HTTP.UTF_8)));
			}

			multipart.addPart("file",
					new FileBody(file, "*/*", HTTP.UTF_8));
			HttpClient client = new DefaultHttpClient();
			post.addHeader("charset", encoding);
			post.setEntity(multipart);
			HttpResponse response = client.execute(post);
			String result = "";
			int statusCode = response.getStatusLine().getStatusCode();

			if (statusCode == 200) {
				result = EntityUtils.toString(response.getEntity(), encoding);
			}

			return result;
		} catch (Exception e) {
			Log.e("error", e.getMessage());
			return "";
		}
	}
}
