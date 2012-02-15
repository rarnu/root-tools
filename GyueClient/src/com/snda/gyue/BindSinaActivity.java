package com.snda.gyue;

import java.util.SortedSet;

import oauth.signpost.OAuthProvider;
import oauth.signpost.basic.DefaultOAuthProvider;
import oauth.signpost.commonshttp.CommonsHttpOAuthConsumer;
import weibo4android.Weibo;
import android.app.Activity;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.Toast;

public class BindSinaActivity extends Activity implements OnClickListener {

	CommonsHttpOAuthConsumer httpOauthConsumer;
	OAuthProvider httpOauthprovider;

	Button btnBack;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		setContentView(R.layout.bind_weibo);

		btnBack = (Button) findViewById(R.id.btnBack);
		btnBack.setOnClickListener(this);

		String callBackUrl = "sina://BindSinaActivity";

		System.setProperty("weibo4j.oauth.consumerKey", SnsKeys.SINA_TOKEN);
		System.setProperty("weibo4j.oauth.consumerSecret", SnsKeys.SINA_SECRET);

		try {
			httpOauthConsumer = new CommonsHttpOAuthConsumer(SnsKeys.SINA_TOKEN, SnsKeys.SINA_SECRET);
			httpOauthprovider = new DefaultOAuthProvider("http://api.t.sina.com.cn/oauth/request_token",
					"http://api.t.sina.com.cn/oauth/access_token", "http://api.t.sina.com.cn/oauth/authorize");
			String authUrl = httpOauthprovider.retrieveRequestToken(httpOauthConsumer, callBackUrl);
			startActivity(new Intent(Intent.ACTION_VIEW, Uri.parse(authUrl)));

		} catch (Exception e) {
			Log.e("SINA WEIBO", e.getMessage());
		}
	}

	@Override
	protected void onNewIntent(Intent intent) {
		super.onNewIntent(intent);

		try {
			Uri uri = intent.getData();
			String verifier = uri.getQueryParameter(oauth.signpost.OAuth.OAUTH_VERIFIER);

			httpOauthprovider.setOAuth10a(true);
			httpOauthprovider.retrieveAccessToken(httpOauthConsumer, verifier);

			SortedSet<String> user_id = httpOauthprovider.getResponseParameters().get("user_id");
			String userId = user_id.first();
			String userKey = httpOauthConsumer.getToken();
			String userSecret = httpOauthConsumer.getTokenSecret();

			GlobalInstance.sinaToken = userKey;
			GlobalInstance.sinaSecret = userSecret;

			System.setProperty("weibo4j.oauth.consumerKey", SnsKeys.SINA_TOKEN);
			System.setProperty("weibo4j.oauth.consumerSecret", SnsKeys.SINA_SECRET);
			Weibo w = new Weibo();
			w.setToken(userKey, userSecret);
			GlobalInstance.sinaName = w.showUser(userId).getName();
			Intent inMain = new Intent(this, MainActivity.class);
			inMain.putExtra("bind", "sina");
			startActivity(inMain);

		} catch (Exception e) {
			Toast.makeText(this, R.string.bind_fail, Toast.LENGTH_LONG).show();
		}

		finish();

	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnBack:
			finish();
			break;
		}

	}
}
