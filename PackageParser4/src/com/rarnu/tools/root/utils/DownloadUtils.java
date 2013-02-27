package com.rarnu.tools.root.utils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.graphics.BitmapFactory;
import android.os.Handler;
import android.os.Message;
import android.widget.ImageView;

public class DownloadUtils {

	public static final int WHAT_DOWNLOAD_START = 1;
	public static final int WHAT_DOWNLOAD_PROGRESS = 2;
	public static final int WHAT_DOWNLOAD_FINISH = 3;

	private static List<String> listDownloading = new ArrayList<String>();

	public static void downloadFileT(final Context context, final ImageView iv,
			final String url, String localDir, final String localFile,
			final Handler hProgress) {

		if (!localDir.endsWith("/")) {
			localDir += "/";
		}
		File fDir = new File(localDir);
		if (!fDir.exists()) {
			fDir.mkdirs();
		}
		final String filePath = localDir + localFile;

		File fImg = new File(filePath);
		if (fImg.exists()) {
			try {
				iv.setImageBitmap(BitmapFactory.decodeFile(filePath));
			} catch (Exception e) {

			}
			return;
		}

		final Handler hImage = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					File file = new File(filePath);
					if (file.exists()) {
						try {
							if (iv != null) {
								iv.setImageBitmap(BitmapFactory
										.decodeFile(filePath));
							}
						} catch (Exception e) {

						}
					}
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {
			@Override
			public void run() {
				downloadFile(url, filePath, hProgress);
				hImage.sendEmptyMessage(1);
			}
		}).start();
	}

	public static void downloadFile(String address, String localFile, Handler h) {
		if (listDownloading.indexOf(localFile) != -1) {
			return;
		}
		listDownloading.add(localFile);

		File fTmp = new File(localFile);
		if (fTmp.exists()) {
			fTmp.delete();
		}

		URL url = null;
		int filesize = 0;
		int position = 0;
		try {
			url = new URL(address);
			HttpURLConnection con = (HttpURLConnection) url.openConnection();
			InputStream in = con.getInputStream();
			filesize = con.getContentLength();
			if (h != null) {
				Message msg = new Message();
				msg.what = WHAT_DOWNLOAD_START;
				msg.arg1 = position;
				msg.arg2 = filesize;
				h.sendMessage(msg);
			}
			File fileOut = new File(localFile + ".tmp");
			FileOutputStream out = new FileOutputStream(fileOut);
			byte[] bytes = new byte[1024];
			int c;
			while ((c = in.read(bytes)) != -1) {
				out.write(bytes, 0, c);
				position += c;

				if (h != null) {
					Message msg = new Message();
					msg.what = WHAT_DOWNLOAD_PROGRESS;
					msg.arg1 = position;
					msg.arg2 = filesize;
					h.sendMessage(msg);
				}
			}
			in.close();
			out.close();
			fileOut.renameTo(fTmp);
			if (h != null) {
				Message msg = new Message();
				msg.what = WHAT_DOWNLOAD_FINISH;
				msg.arg1 = 0;
				msg.arg2 = filesize;
				h.sendMessage(msg);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		listDownloading.remove(localFile);
	}

}
