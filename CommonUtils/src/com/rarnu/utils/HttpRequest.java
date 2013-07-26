package com.rarnu.utils;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.http.HttpResponse;
import org.apache.http.client.CookieStore;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.util.EntityUtils;

import com.rarnu.utils.common.HttpRequestResponseData;
import com.rarnu.utils.http.FilePart;
import com.rarnu.utils.http.MultipartEntity;
import com.rarnu.utils.http.Part;
import com.rarnu.utils.http.StringPart;

public class HttpRequest {

	public static String simplePostWithHeader(String host, String param,
			String encoding, Map<String, String> property) throws Exception {
		URL url = new URL(host);
		URLConnection conn = url.openConnection();
		Iterator<Entry<String, String>> iter = property.entrySet().iterator();
		while (iter.hasNext()) {
			Map.Entry<String, String> entry = iter.next();
			conn.addRequestProperty(entry.getKey(), entry.getValue());
		}
		conn.setDoOutput(true);
		OutputStreamWriter osw = new OutputStreamWriter(conn.getOutputStream());
		osw.write(param);
		osw.flush();
		osw.close();
		InputStreamReader isr = new InputStreamReader(conn.getInputStream(),
				encoding);
		BufferedReader br = new BufferedReader(isr);
		String content = null;
		String result = "";
		while ((content = br.readLine()) != null) {
			result += content + "\n";
		}
		br.close();
		isr.close();
		return result;
	}

	public static String simplePost(String host, String param, String encoding)
			throws Exception {
		return simplePostWithHeader(host, param, encoding, null);
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

	public static String postFile(String host, List<BasicNameValuePair> params,
			List<BasicNameValuePair> files, String encoding) {
		HttpPost httpPost = buildPostFileParts(host, params, files, encoding);
		return executeForResult(httpPost, encoding);
	}

	public static HttpRequestResponseData postWithHeader(String host,
			List<BasicNameValuePair> params, CookieStore cookie, String encoding) {
		HttpPost httpPost = new HttpPost(host);
		try {
			UrlEncodedFormEntity p_entity = new UrlEncodedFormEntity(params,
					encoding);
			httpPost.setEntity(p_entity);
		} catch (UnsupportedEncodingException e) {

		}
		return executeForData(httpPost, cookie, encoding);
	}

	public static HttpRequestResponseData postFileWithHeader(String host,
			List<BasicNameValuePair> params, List<BasicNameValuePair> files,
			CookieStore cookie, String encoding) {
		HttpPost httpPost = buildPostFileParts(host, params, files, encoding);
		return executeForData(httpPost, cookie, encoding);
	}

	private static HttpPost buildPostFileParts(String host,
			List<BasicNameValuePair> params, List<BasicNameValuePair> files,
			String encoding) {
		HttpPost httpPost = new HttpPost(host);
		try {
			Part[] p = new Part[params.size() + files.size()];
			for (int i = 0; i < params.size(); i++) {
				p[i] = new StringPart(params.get(i).getName(), params.get(i)
						.getValue(), encoding);
			}
			int idx = params.size();
			for (int i = idx; i < p.length; i++) {
				p[i] = new FilePart(files.get(i - idx).getName(), new File(
						files.get(i - idx).getValue()), "*/*", encoding);
			}
			MultipartEntity multipart = new MultipartEntity(p);
			httpPost.setEntity(multipart);
		} catch (Exception e) {

		}
		return httpPost;
	}

	public static String get(String host, String params, String encoding) {
		HttpGet request = new HttpGet(host + "?" + params);
		return executeForResult(request, encoding);
	}

	public static HttpRequestResponseData getWithHeader(String host,
			String params, CookieStore cookie, String encoding) {
		HttpGet request = new HttpGet(host + "?" + params);
		return executeForData(request, cookie, encoding);
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

	private static HttpRequestResponseData executeForData(
			HttpRequestBase request, CookieStore cookie, String encoding) {
		HttpRequestResponseData data = null;
		try {
			DefaultHttpClient client = new DefaultHttpClient(buildHttpParams());
			if (cookie != null) {
				client.setCookieStore(cookie);
			}
			HttpResponse response = client.execute(request);
			int statusCode = response.getStatusLine().getStatusCode();
			if (statusCode == 200) {
				data = new HttpRequestResponseData();
				data.data = EntityUtils
						.toString(response.getEntity(), encoding);
				data.cookie = client.getCookieStore();
			}
		} catch (Exception e) {

		}
		return data;
	}

}
