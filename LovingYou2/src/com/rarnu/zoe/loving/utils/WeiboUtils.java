package com.rarnu.zoe.loving.utils;

import com.rarnu.zoe.loving.common.Config;
import com.weibo.sdk.android.Oauth2AccessToken;
import com.weibo.sdk.android.api.StatusesAPI;
import com.weibo.sdk.android.net.RequestListener;

public class WeiboUtils {

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
}
