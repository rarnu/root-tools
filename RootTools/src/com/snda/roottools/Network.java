package com.snda.roottools;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.params.CoreConnectionPNames;
import org.apache.http.util.EntityUtils;


public class Network {

	public static String CallGet(String url, String params, String encoding)
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

}
