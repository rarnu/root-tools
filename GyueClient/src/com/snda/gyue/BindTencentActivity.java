package com.snda.gyue;

import android.app.Activity;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.Toast;

import com.tencent.weibo.beans.OAuth;
import com.tencent.weibo.utils.OAuthClient;

public class BindTencentActivity extends Activity implements OnClickListener {

	OAuth oauth;
	OAuthClient auth;

	Button btnBack;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		setContentView(R.layout.bind_weibo);

		btnBack = (Button) findViewById(R.id.btnBack);
		btnBack.setOnClickListener(this);

		try {
			oauth = new OAuth("tencent://BindTencentActivity");
			auth = new OAuthClient();
			oauth = auth.requestToken(oauth);
			String oauth_token = oauth.getOauth_token();
			String url = "http://open.t.qq.com/cgi-bin/authorize?oauth_token=" + oauth_token;
			Uri uri = Uri.parse(url);
			startActivity(new Intent(Intent.ACTION_VIEW, uri));
		} catch (Exception e) {
			Log.e("TENCENT WEIBO", e.getMessage());
			Toast.makeText(this, R.string.bind_fail, Toast.LENGTH_LONG).show();
			finish();
		}
	}

	@Override
	protected void onNewIntent(Intent intent) {
		super.onNewIntent(intent);

		try {

			Uri uri = intent.getData();

			String oauth_verifier = uri.getQueryParameter("oauth_verifier");
			String oauth_token = uri.getQueryParameter("oauth_token");
			oauth.setOauth_verifier(oauth_verifier);
			oauth.setOauth_token(oauth_token);
			oauth = auth.accessToken(oauth);

			String userKey = oauth.getOauth_token();
			String userSecret = oauth.getOauth_token_secret();

			GlobalInstance.tencentToken = userKey;
			GlobalInstance.tencentSecret = userSecret;
			GlobalInstance.tencentName = auth.getAccount(oauth).getNick();

			Intent inMain = new Intent(this, MainActivity.class);
			inMain.putExtra("bind", "tencent");
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
