package com.rarnu.zoe.love2.utils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.graphics.BitmapFactory;
import android.os.Environment;
import android.os.Handler;
import android.os.Message;
import android.view.View;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import com.rarnu.zoe.love2.common.GroundInfo;

public class DownloadUtils {

	private static List<String> listDownloading = new ArrayList<String>();

	public static String SAVE_PATH = Environment.getExternalStorageDirectory()
			.getPath() + "/.lovingyou2/";

	static {
		File fSave = new File(SAVE_PATH);
		if (!fSave.exists()) {
			fSave.mkdirs();
		}
	}

	/**
	 * 
	 * @param context
	 * @param ground
	 * @param iv
	 * @param type
	 *            0:small 1:big
	 */
	public static void downloadFileT(final Context context,
			final GroundInfo ground, final ImageView iv, final int type,
			final RelativeLayout layLoading) {

		final String filePath = SAVE_PATH
				+ String.format("%s_%s.jpg", (type == 0 ? "t" : "b"), ground.id);
		File fImg = new File(filePath);
		if (fImg.exists()) {
			try {
				iv.setImageBitmap(BitmapFactory.decodeFile(filePath));
			} catch (Exception e) {

			}
			return;
		}

		if (layLoading != null) {
			try {
				layLoading.setVisibility(View.VISIBLE);
			} catch (Exception e) {

			}
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
					if (layLoading != null) {
						try {
							layLoading.setVisibility(View.GONE);
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
				downloadFile(context, (type == 0 ? ground.thumb_path
						: ground.origin_path), String.format("%s_%s.jpg",
						(type == 0 ? "t" : "b"), ground.id));
				h.sendEmptyMessage(1);
			}
		}).start();
	}

	public static void downloadFile(Context context, String address,
			String localPureName) {

		if (listDownloading.indexOf(localPureName) != -1) {
			return;
		}
		listDownloading.add(localPureName);

		File fTmp = new File(SAVE_PATH + localPureName);
		if (fTmp.exists()) {
			fTmp.delete();
		}
		URL url = null;
		try {
			url = new URL(address);
			HttpURLConnection con = (HttpURLConnection) url.openConnection();
			InputStream in = con.getInputStream();
			File fileOut = new File(SAVE_PATH + localPureName + ".tmp");
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
		listDownloading.remove(localPureName);
	}

}
