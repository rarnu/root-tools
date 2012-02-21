package com.snda.gyue.utils;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URLEncoder;

import org.apache.http.protocol.HTTP;

import weibo4android.Weibo;
import weibo4android.http.ImageItem;

import android.util.Log;

import com.snda.gyue.GlobalInstance;
import com.snda.gyue.GyueConsts;
import com.snda.gyue.SnsKeys;
import com.snda.gyue.classes.ArticleItem;
import com.tencent.weibo.api.T_API;
import com.tencent.weibo.beans.OAuth;
import com.tencent.weibo.utils.Configuration;
import com.tencent.weibo.utils.OAuthClient;

public class ShareUtils {

	public static boolean shareArticleToSina(ArticleItem item, String text) {
		// share to sina

		boolean ret = false;
		try {
			System.setProperty("weibo4j.oauth.consumerKey", SnsKeys.SINA_TOKEN);
			System.setProperty("weibo4j.oauth.consumerSecret", SnsKeys.SINA_SECRET);
			Weibo w = new Weibo();
			w.setToken(GlobalInstance.sinaToken, GlobalInstance.sinaSecret);
			if (GlobalInstance.shareWithPic) {
				String fn = GyueConsts.GYUE_DIR + item.getArticleImageLocalFileName();
				byte[] content = readFileImage(fn);
				ImageItem pic = new ImageItem("pic", content);
				w.uploadStatus(URLEncoder.encode(text, HTTP.UTF_8), pic);
				ret = true;
			} else {
				w.updateStatus(URLEncoder.encode(text, HTTP.UTF_8));
				ret = true;
			}
		} catch (Exception e) {

		}

		return ret;
	}

	public static boolean shareArticleToTencent(ArticleItem item, String text) {
		// share to tencent
		boolean ret = false;
		try {
			OAuth oauth = new OAuth("tencent://BindTencentActivity");
			OAuthClient auth = new OAuthClient();
			oauth = auth.requestToken(oauth);

			oauth.setOauth_token(GlobalInstance.tencentToken);
			oauth.setOauth_token_secret(GlobalInstance.tencentSecret);

			T_API t = new T_API();
			if (GlobalInstance.shareWithPic) {
				t.add_pic(oauth, "json", text, Configuration.wifiIp,
						GyueConsts.GYUE_DIR + item.getArticleImageLocalFileName());
			} else {
				t.add(oauth, "json", text, Configuration.wifiIp);
			}
			ret = true;
		} catch (Exception e) {
			Log.e("TENCENT SHARE", e.getMessage());
		}

		return ret;
	}

	private static byte[] readFileImage(String filename) throws IOException {
		BufferedInputStream bufferedInputStream = new BufferedInputStream(new FileInputStream(filename));
		int len = bufferedInputStream.available();
		byte[] bytes = new byte[len];
		int r = bufferedInputStream.read(bytes);
		if (len != r) {
			bytes = null;
			throw new IOException("读取文件不正确");
		}
		bufferedInputStream.close();
		return bytes;
	}
}
