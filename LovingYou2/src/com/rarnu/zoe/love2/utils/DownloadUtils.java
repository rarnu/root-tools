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
import android.widget.ImageView;

import com.rarnu.zoe.love2.common.GroundInfo;

public class DownloadUtils {

	private static List<String> listDownloading = new ArrayList<String>();

	private static String savePath = Environment.getExternalStorageDirectory()
			.getPath() + "/.lovingyou2/";

	static {
		File fSave = new File(savePath);
		if (!fSave.exists()) {
			fSave.mkdirs();
		}
	}

	public static void downloadFileT(final Context context,
			final GroundInfo ground, final ImageView iv) {

		final String filePath = savePath + String.format("%s.jpg", ground.id);
		File fImg = new File(filePath);
		if (fImg.exists()) {
			iv.setImageBitmap(BitmapFactory.decodeFile(filePath));
			return;
		}

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					File file = new File(filePath);
					if (file.exists()) {
						iv.setImageBitmap(BitmapFactory.decodeFile(filePath));
					}
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {
			@Override
			public void run() {
				downloadFile(context, ground.thumb_path,
						String.format("%s.jpg", ground.id));
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

		File fTmp = new File(savePath + localPureName);
		if (fTmp.exists()) {
			fTmp.delete();
		}
		URL url = null;
		try {
			url = new URL(address);
			HttpURLConnection con = (HttpURLConnection) url.openConnection();
			InputStream in = con.getInputStream();
			File fileOut = new File(savePath + localPureName + ".tmp");
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
