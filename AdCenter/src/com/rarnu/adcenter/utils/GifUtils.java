package com.rarnu.adcenter.utils;

import java.io.File;
import java.io.FileInputStream;

import android.graphics.BitmapFactory;
import android.os.Handler;
import android.os.Message;

import com.rarnu.devlib.component.GifView;
import com.rarnu.devlib.component.GifView.GifImageType;
import com.rarnu.utils.DownloadUtils;

public class GifUtils {

	public static void loadGifImage(final String url, final GifView gifView) {

		final String localFile = CacheUtils.CACHE_PATH
				+ MiscUtils.getFileName(url);
		if (new File(localFile).exists()) {
			loadGifFile(localFile, gifView);
			return;
		}
		final Handler hLoad = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					loadGifFile(localFile, gifView);
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {
			@Override
			public void run() {
				DownloadUtils.downloadFile(url, localFile, null);
				hLoad.sendEmptyMessage(1);
			}
		}).start();

	}

	private static void loadGifFile(String filePath, GifView gifView) {
		final String ext = MiscUtils.getFileExtension(filePath).toLowerCase();
		gifView.setGifImageType(GifImageType.SYNC_DECODER);
		if (ext.equals("gif")) {
			try {
				gifView.showAnimation();
				gifView.setGifImage(new FileInputStream(filePath));
			} catch (Exception e) {
			}
		} else {
			gifView.showCover();
			gifView.setImageBitmap(BitmapFactory.decodeFile(filePath));
		}
	}
}
