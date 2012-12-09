package com.rarnu.zoe.sinatoken;

import android.app.Activity;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.preference.PreferenceManager;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.TextView;

import com.rarnu.zoe.sinatoken.api.LovingYouApi;
import com.weibo.sdk.android.Weibo;
import com.weibo.sdk.android.WeiboAuthListener;
import com.weibo.sdk.android.WeiboDialogError;
import com.weibo.sdk.android.WeiboException;

public class MainActivity extends Activity implements WeiboAuthListener,
		OnClickListener {

	private final String appKey = "2002030520";
	private final String redirectUrl = "http://rarnu.com";

	private Weibo weibo;

	Button btnRenew;
	TextView tvCurrentToken, tvNewToken;

	final Handler h = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				runOnUiThread(new Runnable() {
					@Override
					public void run() {
						showNewToken();
					}
				});
			}
			super.handleMessage(msg);
		}
	};

	private String token = "", expiredTime = "";
	private String newToken = "", newExpiredTime = "";

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_main);

		btnRenew = (Button) findViewById(R.id.btnRenew);
		tvCurrentToken = (TextView) findViewById(R.id.tvCurrentToken);
		tvNewToken = (TextView) findViewById(R.id.tvNewToken);

		loadToken();
		showCurrentToken();

		btnRenew.setOnClickListener(this);

	}

	private void showCurrentToken() {
		tvCurrentToken.setText(String.format(
				getString(R.string.current_token_fmt), token, expiredTime));
	}

	private void showNewToken() {
		tvNewToken.setText(String.format(getString(R.string.current_token_fmt),
				newToken, newExpiredTime));
	}

	@Override
	public void onCancel() {

	}

	@Override
	public void onComplete(Bundle values) {
		newToken = values.getString("access_token");
		newExpiredTime = values.getString("expires_in");
		saveToken();
		doUpdateTokenT(newToken, newExpiredTime);

	}

	@Override
	public void onError(WeiboDialogError e) {

	}

	@Override
	public void onWeiboException(WeiboException e) {

	}

	private void loadToken() {
		SharedPreferences sp = PreferenceManager
				.getDefaultSharedPreferences(this);
		token = sp.getString("token", "");
		expiredTime = sp.getString("expired_time", "");
	}

	private void saveToken() {
		SharedPreferences sp = PreferenceManager
				.getDefaultSharedPreferences(this);
		sp.edit().putString("token", newToken).commit();
		sp.edit().putString("expired_time", newExpiredTime).commit();

	}

	@Override
	public void onClick(View v) {
		weibo = Weibo.getInstance(appKey, redirectUrl);
		weibo.authorize(this, this);

	}

	private void doUpdateTokenT(final String token, final String time) {
		LovingYouApi.updateToken(token, time);
		h.sendEmptyMessage(1);
	}

}
