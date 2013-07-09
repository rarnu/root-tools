package com.sbbs.me.android.utils;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.apache.http.protocol.HTTP;
import org.eclipse.egit.github.core.User;
import org.eclipse.egit.github.core.client.GitHubClient;
import org.eclipse.egit.github.core.service.UserService;
import org.json.JSONObject;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.os.Environment;
import android.util.Log;

import com.rarnu.utils.DownloadUtils;
import com.rarnu.utils.HttpRequest;
import com.rarnu.utils.ImageUtils;
import com.sbbs.me.android.api.SbbsMeGoogleUser;
import com.sbbs.me.android.dialog.GithubOAuthDialog;

public class GithubOAuth {

	public interface GithubUserCallback {
		void onGetGithubUser(User user);
	}
	
	private Context mContext;
	public SbbsMeGoogleUser githubUser = null;
	private GithubUserCallback callback;
	private final String clientId = "afc5146525f2f6342057";
	private final String clientSecret = "7fa3196428c6051e293c3a931935c6b3da87d6a1";
	private final String callbackUrl = "http://sbbs.me/auth/github_oauth2/callback";
	String githubTokenApi = "https://github.com/login/oauth/access_token";
	String githubUserApi = "https://api.github.com/user";
	
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
				Map<String, String> property = new HashMap<String, String>();
				property.put("accept", "application/json");
				String access_token = "";
				String params = "client_id=" + clientId + "&client_secret=" 
						+ clientSecret + "&code=" + code;
				String ret = "";
				try {
					ret = HttpRequest.simplePostWithHeader(githubTokenApi, 
							params, HTTP.UTF_8, property);
				} catch (Exception e) {
				}
				try {
					JSONObject s = new JSONObject(ret);
					access_token = s.getString("access_token");
				} catch (Exception e) {
				}
				GitHubClient client = new GitHubClient();
				client.setOAuth2Token(access_token);
				UserService us = new UserService(client);
				User user = null;
				try {
					user = us.getUser();
				} catch (IOException e) {
				}
				Config.setGithubUserId(mContext, 
						String.valueOf(user.getId()), access_token);
				if (callback != null) {
					callback.onGetGithubUser(user);
				}
			}
		}).start();
	}
	
	public Drawable getUserHead(String url) {
		Log.e("head-url", url);
		String headLocalPath = Environment.getExternalStorageDirectory()
				.getPath() + "/.sbbs/";
		if (!new File(headLocalPath).exists()) {
			new File(headLocalPath).mkdirs();
		}
		String headLocalName = headLocalPath + "mygithubhead.jpg";

		if (!new File(headLocalName).exists()) {
			DownloadUtils.downloadFile(url, headLocalName, null);
		}

		Drawable d = null;
		try {
			d = Drawable.createFromPath(headLocalName);
			d = ImageUtils.zoomDrawable(d, 256, 256);
		} catch (Exception e) {

		}
		return d;
	}
}
