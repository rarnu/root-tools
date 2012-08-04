package com.rarnu.findaround.common;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.List;

import android.content.Context;
import android.graphics.drawable.BitmapDrawable;
import android.os.Environment;
import android.os.Handler;
import android.os.Message;
import android.widget.AbsListView;
import android.widget.AbsListView.LayoutParams;
import android.widget.ListView;

import com.rarnu.findaround.comp.MapHead;

public class NetFiles {

	public static String CACHE_DIR = Environment.getExternalStorageDirectory()
			.getPath() + "/.cache/";

	static {
		File fCache = new File(CACHE_DIR);
		if (!fCache.exists()) {
			fCache.mkdirs();
		}
	}

	public static void doDownloadImagePackT(final Context context,
			final List<String> address, final Handler hPack) {
		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					hPack.sendEmptyMessage(99);
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				String local = "";
				for (int i = 0; i < address.size(); i++) {
					local = buildLocalFileName(address.get(i));
					downloadFile(h, address.get(i), local);
					h.sendEmptyMessage(1);
				}

			}
		}).start();
	}

	public static String buildLocalFileName(String url) {
		String ret = "";
		for (int i = url.length() - 1; i >= 0; i--) {
			if (url.charAt(i) != '/') {
				ret = url.charAt(i) + ret;
			} else {
				break;
			}
		}
		return CACHE_DIR + ret;
	}

	public static void doDownloadImageT(final Context context,
			final String address, final String savePath, final MapHead imgView,
			final ListView lv) {
		final String localPath = CACHE_DIR + savePath;

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {

					File fImg = new File(localPath);
					if (fImg.exists()) {
						imgView.setBackgroundDrawable(BitmapDrawable
								.createFromPath(fImg.getAbsolutePath()));
						imgView.setLayoutParams(new AbsListView.LayoutParams(
								LayoutParams.MATCH_PARENT, UIUtils.dipToPx(200)));
						imgView.setCenterVisible(true);
					}
					if (lv != null) {
						lv.postInvalidate();
					}

				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				downloadFile(h, address, localPath);
				h.sendEmptyMessage(1);

			}
		}).start();
	}

	public static void downloadFile(Handler h, String address, String savePath) {
		File fTmp = new File(savePath);
		if (fTmp.exists()) {
			fTmp.delete();
		}
		URL url = null;
		try {
			url = new URL(address);
			HttpURLConnection con = (HttpURLConnection) url.openConnection();
			InputStream in = con.getInputStream();
			File fileOut = new File(savePath + ".tmp");
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
	}

}
