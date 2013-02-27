package com.rarnu.tools.root.utils;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Environment;
import android.util.Log;
import android.widget.Toast;

import com.rarnu.root.pp4.R;

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

}
