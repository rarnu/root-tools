package com.rarnu.devlib.utils;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Environment;
import android.util.Log;
import android.widget.Toast;

import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.devlib.R;

public class MiscUtils {

	public static void doScanMedia(Context context) {
		context.sendBroadcast(new Intent(Intent.ACTION_MEDIA_SCANNER_SCAN_FILE,
				Uri.parse("file://"
						+ Environment.getExternalStorageDirectory()
								.getAbsolutePath())));
		Log.e("Media Path", Environment.getExternalStorageDirectory()
				.getAbsolutePath());
		Toast.makeText(context, R.string.scan, Toast.LENGTH_LONG).show();
	}

	public static boolean isSDCardExists() {
		return (Environment.getExternalStorageState()
				.equals(Environment.MEDIA_MOUNTED));
	}

	public static String getSecondSdcardPath(boolean hasSplit) {
		CommandResult ret = RootUtils.runCommand("mount", false);
		String[] lines = ret.result.split("\n");

		String systemSdcard = Environment.getExternalStorageDirectory()
				.getAbsolutePath();
		String extSdcard = "";

		for (String s : lines) {
			if (s.contains("secure") || s.contains("asec")) {
				continue;
			}
			if (s.contains("fat") || s.contains("fuse")) {
				String columns[] = s.split(" ");
				if (columns != null && columns.length > 3) {
					if (columns[3].contains("rw,")) {
						extSdcard += columns[1] + "\n";
					}
				}
			}
		}
		extSdcard = extSdcard.replace(systemSdcard, "").trim();
		if (!extSdcard.endsWith("/") && hasSplit) {
			extSdcard += "/";
		}
		return extSdcard;
	}
}
