package com.snda.gyue.utils;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URLEncoder;

import org.apache.http.protocol.HTTP;

import weibo4android.Weibo;
import weibo4android.http.ImageItem;

import com.snda.gyue.GlobalInstance;
import com.snda.gyue.GyueConsts;
import com.snda.gyue.SnsKeys;
import com.snda.gyue.classes.ArticleItem;

public class ShareUtils {

	public static boolean shareArticleToSina(ArticleItem item) {
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
				w.uploadStatus(URLEncoder.encode(item.getDescription().substring(0, 100), HTTP.UTF_8), pic);
				ret = true;
			} else {
				w.updateStatus(URLEncoder.encode(item.getDescription().substring(0, 100), HTTP.UTF_8));
				ret = true;
			}
		} catch (Exception e) {

		}

		return ret;
	}

	public static boolean shareArticleToTencent(ArticleItem item) {
		// TODO: share to tencent
		return false;
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
