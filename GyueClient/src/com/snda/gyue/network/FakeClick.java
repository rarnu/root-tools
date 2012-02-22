package com.snda.gyue.network;

import java.util.List;
import java.util.Random;

import com.snda.gyue.classes.ArticleItem;

public class FakeClick {

	private static final String FAKE_URL = "http://www.gyue.cn/api.php?op=count&id=%s&modelid=1";

	public static void doFakeClick(String uid) {
		// http://www.gyue.cn/api.php?op=count&id=%s&modelid=1
		final String url = String.format(FAKE_URL, uid);
		final int count = new Random(System.currentTimeMillis()).nextInt(20);
		new Thread(new Runnable() {

			@Override
			public void run() {
				for (int i = 0; i < count; i++) {
					try {
						HttpProxy.CallGet(url, "", "gbk");
					} catch (Exception e) {

					}
				}

			}
		}).start();
	}

	public static void doFakeClickAll(final List<ArticleItem> list) {
		new Thread(new Runnable() {

			@Override
			public void run() {
				String url = "";
				int count = 0;
				for (int i = 0; i < list.size(); i++) {
					url = String.format(FAKE_URL, list.get(i).getUid());
					count = new Random(System.currentTimeMillis()).nextInt(20);
					for (int j = 0; j < count; j++) {
						try {
							HttpProxy.CallGet(url, "", "gbk");
						} catch (Exception e) {

						}
					}
				}

			}
		}).start();
	}
}
