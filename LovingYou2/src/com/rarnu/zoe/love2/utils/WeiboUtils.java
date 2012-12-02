package com.rarnu.zoe.love2.utils;

import java.io.IOException;

import com.rarnu.zoe.love2.common.Config;
import com.weibo.sdk.android.Oauth2AccessToken;
import com.weibo.sdk.android.WeiboException;
import com.weibo.sdk.android.api.StatusesAPI;
import com.weibo.sdk.android.net.RequestListener;

public class WeiboUtils implements RequestListener {

	public static void shareArticleToSina(String text, String file,
			RequestListener listener) {
		StatusesAPI api = new StatusesAPI(new Oauth2AccessToken(Config.TOKEN,
				Config.EXPRIED));
		
		if (file == null || file.equals("")) {
			api.update(text, "0", "0", listener);
		} else {
			api.upload(text, file, "0", "0", listener);
		}
	}
	
	public void getUserTimeLine() {
		StatusesAPI api = new StatusesAPI(new Oauth2AccessToken(Config.TOKEN,
				Config.EXPRIED));
		api.friendsTimeline(0L, 0L, 1, 1, true, null, true, this);
	}

	@Override
	public void onComplete(String arg0) {
		StatusesAPI api = new StatusesAPI(new Oauth2AccessToken(Config.TOKEN,
				Config.EXPRIED));
		
		
	}

	@Override
	public void onError(WeiboException arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void onIOException(IOException arg0) {
		// TODO Auto-generated method stub
		
	}
}
