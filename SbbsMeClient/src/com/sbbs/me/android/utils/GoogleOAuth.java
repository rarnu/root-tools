package com.sbbs.me.android.utils;

import org.apache.http.protocol.HTTP;
import org.json.JSONObject;

import android.content.Context;
import android.graphics.drawable.Drawable;

import com.rarnu.utils.HttpRequest;
import com.sbbs.me.android.api.SbbsMeGoogleUser;
import com.sbbs.me.android.dialog.GoogleOAuthDialog;

public class GoogleOAuth {

	public interface GoogleUserCallback {
		void onGetGoogleUser(SbbsMeGoogleUser user);
	}

	private Context mContext;
	public SbbsMeGoogleUser sinaUser = null;
	private GoogleUserCallback callback;
	private final String clientId = "876356697116.apps.googleusercontent.com";
	private final String callbackUrl = "http://sbbs.me/auth/google_oauth2/callback";
	String googleUserApi = "https://www.googleapis.com/oauth2/v1/userinfo";

	public GoogleOAuth(Context context, GoogleUserCallback callback) {
		this.mContext = context;
		this.callback = callback;
	}

	public void sendGoogleOauth() {
		GoogleOAuthDialog dialog = new GoogleOAuthDialog(mContext, clientId,
				callbackUrl, this);
		dialog.show();
	}

	public void getGoogleUserInfoViaOAuth() {
		new GoogleOAuthDialog(mContext, clientId, callbackUrl, this);
	}

	public void getGoogleUserInfo(final String token) {

		new Thread(new Runnable() {
			@Override
			public void run() {
				String ret = HttpRequest.get(googleUserApi, "access_token="
						+ token, HTTP.UTF_8);
				SbbsMeGoogleUser user = null;
				try {
					user = SbbsMeGoogleUser.fromJson(new JSONObject(ret));
					Config.setGoogleUserId(mContext, user.id, token);
					Config.setUserName(mContext, user.name);
					Config.setAvatarUrl(mContext, user.picture);
				} catch (Exception e) {

				}
				if (callback != null) {
					callback.onGetGoogleUser(user);
				}
			}
		}).start();

	}

	public Drawable getUserHead(String url) {
		return MiscUtils.getUserHead(mContext, url, "mygooglehead.jpg");
	}
}
