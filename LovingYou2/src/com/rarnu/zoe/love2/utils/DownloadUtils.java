package com.rarnu.zoe.love2.utils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.content.Intent;
import android.util.Log;

public class DownloadUtils {

	private static List<String> listDownloading = new ArrayList<String>();

	public static void downloadFile(Context context, String address,
			String savePath, String localPureName, String broadcastAction) {
		if (listDownloading.indexOf(localPureName) != -1) {
			return;
		}
		listDownloading.add(localPureName);
		
		Log.e("download", String.format("addr:%s, save:%s, act:%s", address, savePath, broadcastAction));
		
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
		listDownloading.remove(localPureName);
		if ((broadcastAction != null) && (!broadcastAction.equals(""))) {
			context.sendBroadcast(new Intent(broadcastAction));
		}
	}

}
