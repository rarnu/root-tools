package com.sbbs.me.android.utils;

import org.apache.http.protocol.HTTP;
import org.json.JSONObject;

import android.content.Context;

import com.rarnu.utils.HttpRequest;
import com.sbbs.me.android.api.SbbsMeGithubUser;
import com.sbbs.me.android.api.SbbsMeGoogleUser;
import com.sbbs.me.android.dialog.GithubOAuthDialog;
import com.sbbs.me.android.dialog.GoogleOAuthDialog;

public class GithubOAuth {

	public interface GithubUserCallback {
		void getGetGithubUser(SbbsMeGithubUser user);
	}
	
	private Context mContext;
	public SbbsMeGoogleUser githubUser = null;
	private GithubUserCallback callback;
	private final String clientId = "afc5146525f2f6342057";
	private final String clientSecret = "7fa3196428c6051e293c3a931935c6b3da87d6a1";
	private final String callbackUrl = "http://sbbs.me/auth/github_oauth2/callback";
	String githubTokenApi = "https://github.com/login/oauth/access_token";
	String githubUserApi = "https://api.github.com/user?access_token=%s";
	//String googleUserApi = "https://www.googleapis.com/oauth2/v1/userinfo";
	
	public GithubOAuth(Context context, GithubUserCallback callback) {
		this.mContext = context;
		this.callback = callback;
	}
	
	public void sendGithubOauth() {
		GithubOAuthDialog dialog = new GithubOAuthDialog(mContext, clientId,
				callbackUrl, this);
		dialog.show();
	}
	
	public void getGithubUserInfoViaOAuth() {
		new GithubOAuthDialog(mContext, clientId, callbackUrl, this);
	}
	
	public void getGithubUserInfo(final String code) {
		new Thread(new Runnable() {
			@Override
			public void run() {
				String access_token = HttpRequest.post(githubTokenApi, "", "UTF-8");
				
			}
		}).start();
	}
}
