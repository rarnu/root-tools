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

	private static List<String> listDownloading = new ArrayList<String>();

	public static void downloadFileT(final Context context, final ImageView iv,
			final String url, final String localFile) {

		final String filePath = DirHelper.ICON_DIR + localFile;

		File fImg = new File(filePath);
		if (fImg.exists()) {
			try {
				iv.setImageBitmap(BitmapFactory.decodeFile(filePath));
			} catch (Exception e) {

			}
			return;
		}

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					File file = new File(filePath);
					if (file.exists()) {
						try {
							iv.setImageBitmap(BitmapFactory
									.decodeFile(filePath));
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
				downloadFile(context, url, filePath);
				h.sendEmptyMessage(1);
			}
		}).start();
	}

	public static void downloadFile(Context context, String address,
			String localFile) {

		if (listDownloading.indexOf(localFile) != -1) {
			return;
		}
		listDownloading.add(localFile);

		File fTmp = new File(localFile);
		if (fTmp.exists()) {
			fTmp.delete();
		}
		URL url = null;
		try {
			url = new URL(address);
			HttpURLConnection con = (HttpURLConnection) url.openConnection();
			InputStream in = con.getInputStream();
			File fileOut = new File(localFile + ".tmp");
			FileOutputStream out = new FileOutputStream(fileOut);
			byte[] bytes = new byte[1024];
			int c;
			while ((c = in.read(bytes)) != -1) {
				out.write(bytes, 0, c);
			}
			in.close();
			out.close();
			fileOut.renameTo(fTmp);
		} catch (Exception e) {
			e.printStackTrace();
		}
		listDownloading.remove(localFile);
	}

}
