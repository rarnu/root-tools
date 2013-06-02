package com.rarnu.utils;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLConnection;
import java.util.List;

import org.apache.http.HttpResponse;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.util.EntityUtils;

public class HttpRequest {

	public static String simplePost(String host, String param, String encoding)
			throws Exception {
		URL url = new URL(host);
		URLConnection conn = url.openConnection();
		conn.setDoOutput(true);
		OutputStreamWriter osw = new OutputStreamWriter(conn.getOutputStream());
		osw.write(param);
		osw.flush();
		osw.close();
		InputStreamReader isr = new InputStreamReader(conn.getInputStream(),
				encoding);
		BufferedReader br = new BufferedReader(isr);
		String content = null;
		String result = null;
		while ((content = br.readLine()) != null) {
			result += content + "\n";
		}
		br.close();
		isr.close();
		return result;
	}

	public static String post(String host, String getParams,
			List<BasicNameValuePair> params, String encoding) {
		String url = host + "?" + getParams;
		return post(url, params, encoding);
	}

	public static String post(String host, List<BasicNameValuePair> params,
			String encoding) {
		HttpPost httpPost = new HttpPost(host);
		try {
			UrlEncodedFormEntity p_entity = new UrlEncodedFormEntity(params,
					encoding);
			httpPost.setEntity(p_entity);
		} catch (UnsupportedEncodingException e) {

		}

		return executeForResult(httpPost, encoding);

	}

	// post a string directly
	public static String post(String host, String param, String encoding) {
		HttpPost httpPost = new HttpPost();
		try {
			StringEntity p_entity = new StringEntity(param, encoding);
			httpPost.setEntity(p_entity);
		} catch (UnsupportedEncodingException e) {
		}

		return executeForResult(httpPost, encoding);
	}

	public static String get(String host, String params, String encoding) {
		HttpGet request = new HttpGet(host + "?" + params);
		return executeForResult(request, encoding);
	}

	private static BasicHttpParams buildHttpParams() {
		BasicHttpParams httpParams = new BasicHttpParams();
		HttpConnectionParams.setConnectionTimeout(httpParams, 5000);
		HttpConnectionParams.setSoTimeout(httpParams, 5000);
		return httpParams;
	}

	private static String executeForResult(HttpRequestBase request,
			String encoding) {
		try {
			DefaultHttpClient client = new DefaultHttpClient(buildHttpParams());
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

}
