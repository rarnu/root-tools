package com.sbbs.me.android.utils;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.apache.http.protocol.HTTP;
import org.eclipse.egit.github.core.client.GitHubClient;
import org.eclipse.egit.github.core.service.UserService;
import org.json.JSONObject;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.util.Log;

import com.rarnu.utils.DownloadUtils;
import com.rarnu.utils.HttpRequest;
import com.sbbs.me.android.api.SbbsMeGithubUser;
import com.sbbs.me.android.consts.PathDefine;
import com.sbbs.me.android.dialog.GithubOAuthDialog;

public class GithubOAuth {

	public interface GithubUserCallback {
		void onGetGithubUser(SbbsMeGithubUser user);
	}

	private Context mContext;
	public SbbsMeGithubUser githubUser = null;
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
				try {
					Map<String, String> property = new HashMap<String, String>();
					property.put("accept", "application/json");
					String access_token = "";
					String params = String.format(
							"client_id=%s&client_secret=%s&code=%s", clientId,
							clientSecret, code);
					String ret = "";
					ret = HttpRequest.simplePostWithHeader(githubTokenApi,
							params, HTTP.UTF_8, property);
					JSONObject s = new JSONObject(ret);
					access_token = s.getString("access_token");
					GitHubClient client = new GitHubClient();
					client.setOAuth2Token(access_token);
					UserService us = new UserService(client);
					githubUser = SbbsMeGithubUser.fromParent(us.getUser());
					Config.setGithubUserId(mContext,
							String.valueOf(githubUser.getId()), access_token);
					Config.setUserName(mContext, githubUser.name);
					Config.setAvatarUrl(mContext, githubUser.avatarUrl);
					if (callback != null) {
						callback.onGetGithubUser(githubUser);
					}
				} catch (Exception e) {
					Log.e("getGithubUserInfo", e.getMessage());
				}
			}
		}).start();
	}

	public Drawable getUserHead(String url) {
		Log.e("head-url", url);
		String headLocalPath = PathDefine.ROOT_PATH;
		if (!new File(headLocalPath).exists()) {
			new File(headLocalPath).mkdirs();
		}
		String headLocalName = headLocalPath + "mygithubhead.jpg";
		Config.setHeadPath(mContext, headLocalName);
		if (!new File(headLocalName).exists()) {
			DownloadUtils.downloadFile(url, headLocalName, null);
		}
		return MiscUtils.loadUserHeadFromFile(headLocalName);
	}
}
