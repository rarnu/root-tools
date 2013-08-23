package com.sbbs.me.android.utils;

import java.io.IOException;

import org.json.JSONException;
import org.json.JSONObject;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.os.Bundle;

import com.sbbs.me.android.api.SbbsMeSinaUser;
import com.weibo.sdk.android.Oauth2AccessToken;
import com.weibo.sdk.android.Weibo;
import com.weibo.sdk.android.WeiboAuthListener;
import com.weibo.sdk.android.WeiboDialogError;
import com.weibo.sdk.android.WeiboException;
import com.weibo.sdk.android.api.AccountAPI;
import com.weibo.sdk.android.api.UsersAPI;
import com.weibo.sdk.android.keep.AccessTokenKeeper;
import com.weibo.sdk.android.net.RequestListener;

public class SinaOAuth {

	public interface SinaUserCallback {
		void onGetSinaUser(SbbsMeSinaUser user);
	}

	private Context mContext;
	private Weibo mWeibo;
	private final String CONSUMER_KEY = "259989602";
	private final String REDIRECT_URL = "http://sbbs.me/weibo/callback";
	public Oauth2AccessToken accessToken;
	public SbbsMeSinaUser sinaUser = null;
	private SinaUserCallback callback;

	public SinaOAuth(Context context, SinaUserCallback callback) {
		this.mContext = context;
		this.callback = callback;
	}

	public void sendSinaOauth() {
		mWeibo = Weibo.getInstance(CONSUMER_KEY, REDIRECT_URL);
		mWeibo.authorize(mContext, new WeiboAuthListener() {

			@Override
			public void onWeiboException(WeiboException error) {

			}

			@Override
			public void onError(WeiboDialogError error) {

			}

			@Override
			public void onComplete(Bundle result) {

				String token = result.getString("access_token");
				String expires_in = result.getString("expires_in");
				accessToken = new Oauth2AccessToken(token, expires_in);
				Config.setAccountType(mContext, 2);
				AccessTokenKeeper.keepAccessToken(mContext, accessToken);
				getSinaAccountInfo();
			}

			@Override
			public void onCancel() {

			}
		});
	}

	public void getSinaAccountInfo() {
		accessToken = AccessTokenKeeper.readAccessToken(mContext);
		AccountAPI api = new AccountAPI(accessToken);
		api.getUid(new RequestListener() {

			@Override
			public void onIOException(IOException error) {

			}

			@Override
			public void onError(WeiboException error) {

			}

			@Override
			public void onComplete(String result) {
				long uid = 0L;
				try {
					JSONObject json = new JSONObject(result);
					uid = json.getLong("uid");
					Config.setSinaUserId(mContext, String.valueOf(uid));
					getSinaUserInfo(String.valueOf(uid));
				} catch (JSONException e) {

				}

			}
		});
	}

	public void getSinaUserInfo(String uid) {
		accessToken = AccessTokenKeeper.readAccessToken(mContext);
		UsersAPI api = new UsersAPI(accessToken);
		long longUid = Long.parseLong(uid);
		api.show(longUid, new RequestListener() {

			@Override
			public void onIOException(IOException error) {

			}

			@Override
			public void onError(WeiboException error) {

			}

			@Override
			public void onComplete(String result) {
				try {
					sinaUser = SbbsMeSinaUser.fromJson(new JSONObject(result));
					Config.setUserName(mContext, sinaUser.screen_name);
					Config.setAvatarUrl(mContext, sinaUser.avatar_large);
				} catch (Exception e) {

				}
				if (callback != null) {
					callback.onGetSinaUser(sinaUser);
				}
			}
		});
	}

	public Drawable getUserHead(String url) {
		return MiscUtils.getUserHead(mContext, url, "mysinahead.jpg");
	}
}
