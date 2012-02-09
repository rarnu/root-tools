package com.snda.gyue.network;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;

import android.content.Context;
import android.os.Handler;
import android.os.Message;
import android.view.View;
import android.widget.ImageView;

import com.snda.gyue.GyueConsts;
import com.snda.gyue.utils.ImageUtils;

public class NetFiles {

	public static void doDownloadImageT(final Context context, final String address, final String savePath,
			final ImageView imgView) {
		final String localPath = GyueConsts.GYUE_DIR + savePath;

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					File fImg = new File(localPath);
					if (fImg.exists()) {
						imgView.setBackgroundDrawable(ImageUtils.loadItemImage(context, localPath));
					} else {
						imgView.setVisibility(View.GONE);
					}
					
					if (imgView.getBackground() == null) {
						imgView.setVisibility(View.GONE);
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
		URL url = null;
		try {
			url = new URL(address);
			HttpURLConnection con = (HttpURLConnection) url.openConnection();
			InputStream in = con.getInputStream();
			File fileOut = new File(savePath);
			FileOutputStream out = new FileOutputStream(fileOut);
			byte[] bytes = new byte[1024];
			int c;
			while ((c = in.read(bytes)) != -1) {
				out.write(bytes, 0, c);
			}
			in.close();
			out.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}
