package com.snda.gyue.network;

import java.io.File;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.entity.FileEntity;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.params.CoreConnectionPNames;
import org.apache.http.util.EntityUtils;

public class HttpProxy {

	public static String CallPost(String url, String params, String encoding)
			throws Exception {

		// generate the request instance and pass in the url
		// url format: http://127.0.0.1:80/android/webservice.aspx/method
		// note: must add method name after the WSDL document
		// and do NOT need any namespace or other information
		HttpPost request = new HttpPost(url);

		// bind method parameters on the request
		StringEntity se = new StringEntity(params);
		request.setEntity(se);

		// execute the request and take the respone
		// if the server is NOT ready, it will return 404 title
		// and it will be filted below.
		HttpResponse response = new DefaultHttpClient().execute(request);

		String result = "";
		int statusCode = response.getStatusLine().getStatusCode();
		if (statusCode == 200) {
			// parse the http response context into pure string
			// and the format of string is standard json
			result = EntityUtils.toString(response.getEntity(), encoding);
		}

		// repackage the string result to JSON object
		return result;
	}

	public static String CallGet(String url, String params, String encoding)
			throws Exception {
		// url format: site-url/action
		// params format: p1=v1&p2=v2&.....
		// so the final url be built will be like this:
		// http://account.everbox.com/login?user=a&passwd=b&devId=c
		HttpGet request = new HttpGet(url + "?" + params);

		// execute the request and take the respone
		// if the server is NOT ready, it will return 404 title
		// and it will be filted below.
		DefaultHttpClient hClient = new DefaultHttpClient();
		hClient.getParams().setParameter(
				CoreConnectionPNames.CONNECTION_TIMEOUT, 10000);
		HttpResponse response = hClient.execute(request);

		String result = "";
		int statusCode = response.getStatusLine().getStatusCode();
		if (statusCode == 200) {
			// parse the http response context into pure string
			// and the format of string is standard json
			
			result = EntityUtils.toString(response.getEntity(), encoding);
		}

		// repackage the string result to JSON object
		// JSONObject json = new JSONObject(result);
		return result;
	}

	public static byte[] CallGetBytes(String url, String params, String encoding)
			throws Exception {
		// url format: site-url/action
		// params format: p1=v1&p2=v2&.....
		// so the final url be built will be like this:
		// http://account.everbox.com/login?user=a&passwd=b&devId=c
		HttpGet request = new HttpGet(url + "?" + params);

		// execute the request and take the respone
		// if the server is NOT ready, it will return 404 title
		// and it will be filted below.
		DefaultHttpClient hClient = new DefaultHttpClient();
		hClient.getParams().setParameter(
				CoreConnectionPNames.CONNECTION_TIMEOUT, 10000);
		HttpResponse response = hClient.execute(request);

		byte[] result = null;
		int statusCode = response.getStatusLine().getStatusCode();
		if (statusCode == 200) {
			// parse the http response context into binary bytes
			result = EntityUtils.toByteArray(response.getEntity());
		}

		// repackage the string result to JSON object
		// JSONObject json = new JSONObject(result);
		return result;
	}

	public static String CallPostBytes(String url, String params, File file,
			String encoding) throws Exception {
		HttpPut request = new HttpPut(url + "?" + params);
		FileEntity fe = new FileEntity(file, "application/octet-stream");
		request.setEntity(fe);
		// upload file by bytes
		DefaultHttpClient hClient = new DefaultHttpClient();
		hClient.getParams().setParameter(
				CoreConnectionPNames.CONNECTION_TIMEOUT, 10000);
		HttpResponse response = hClient.execute(request);

		String result = "";
		int statusCode = response.getStatusLine().getStatusCode();
		if (statusCode == 200) {
			// parse the http response context into pure string
			// and the format of string is standard json
			result = EntityUtils.toString(response.getEntity());
		}

		// repackage the string result to JSON object
		// JSONObject json = new JSONObject(result);
		return result;
	}

}
