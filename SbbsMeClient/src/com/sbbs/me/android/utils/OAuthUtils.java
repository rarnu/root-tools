package com.sbbs.me.android.utils;

import android.content.Context;
import android.os.Bundle;

import com.weibo.sdk.android.Oauth2AccessToken;
import com.weibo.sdk.android.Weibo;
import com.weibo.sdk.android.WeiboAuthListener;
import com.weibo.sdk.android.WeiboDialogError;
import com.weibo.sdk.android.WeiboException;

public class OAuthUtils {

	private static Context mContext;
	private static Weibo mWeibo;
	private static final String CONSUMER_KEY = "259989602";
	private static final String REDIRECT_URL = "http://sbbs.me/weibo/callback";
	public static Oauth2AccessToken accessToken;

	public static void sendSinaOauth(Context context) {
		mContext = context;
		mWeibo = Weibo.getInstance(CONSUMER_KEY, REDIRECT_URL);
		mWeibo.authorize(context, new AuthDialogListener());
	}

	public static class AuthDialogListener implements WeiboAuthListener {

		@Override
		public void onCancel() {

		}

		@Override
		public void onComplete(Bundle values) {
			String token = values.getString("access_token");
			String expires_in = values.getString("expires_in");
			accessToken = new Oauth2AccessToken(token, expires_in);
			if (accessToken.isSessionValid()) {
				AccessTokenKeeper.keepAccessToken(mContext, accessToken);
			}
		}

		@Override
		public void onError(WeiboDialogError arg0) {

		}

		@Override
		public void onWeiboException(WeiboException arg0) {

		}

	}
}
