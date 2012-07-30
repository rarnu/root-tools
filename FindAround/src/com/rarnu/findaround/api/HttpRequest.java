package com.rarnu.findaround.api;

import java.util.List;

import org.apache.http.HttpResponse;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.util.EntityUtils;

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

		// url format: site-url/action
		// params format: p1=v1&p2=v2&.....
		// so the final url be built will be like this:
		// http://account.everbox.com/login?user=a&passwd=b&devId=c
		// try {
		// params = URLEncoder.encode(params, encoding);
		// } catch (UnsupportedEncodingException e1) {
		// }
		HttpGet request = new HttpGet(host + "?" + params);

		try {

			// execute the request and take the respone
			// if the server is NOT ready, it will return 404 title
			// and it will be filted below.
			BasicHttpParams httpParams = new BasicHttpParams();
			HttpConnectionParams.setConnectionTimeout(httpParams, 5000);
			HttpConnectionParams.setSoTimeout(httpParams, 5000);

			DefaultHttpClient client = new DefaultHttpClient(httpParams);
			HttpResponse response = client.execute(request);

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
		} catch (Exception e) {
			return "";
		}
	}
}
