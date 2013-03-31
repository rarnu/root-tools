package com.rarnu.almanac.utils;

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.net.Uri;

import com.rarnu.almanac.R;
import com.rarnu.almanac.api.MobileApi;
import com.rarnu.almanac.api.UpdateInfo;

public class UpdateUtils {

	public static void showUpdateInfo(final Context context,
			UpdateInfo updateInfo) {

		if (updateInfo == null || updateInfo.result == 0) {
			noUpdate(context);
		} else {
			showUpdate(context, updateInfo);
		}
	}

	private static void noUpdate(Context context) {
		new AlertDialog.Builder(context).setTitle(R.string.check_update)
				.setMessage(R.string.no_update_found).setCancelable(false)
				.setPositiveButton(R.string.ok, null).show();
	}

	private static void showUpdate(final Context context, final UpdateInfo updateInfo) {
		new AlertDialog.Builder(context)
				.setTitle(R.string.check_update)
				.setCancelable(false)
				.setMessage(
						String.format(
								context.getString(R.string.update_found_info),
								updateInfo.versionName, updateInfo.size))
				.setPositiveButton(R.string.ok,
						new DialogInterface.OnClickListener() {

							@Override
							public void onClick(DialogInterface dialog,
									int which) {
								sendDownloadIntent(context, updateInfo);

							}
						}).setNegativeButton(R.string.cancel, null).show();
	}

	private static void sendDownloadIntent(Context context, UpdateInfo updateInfo) {
		// download new version
		String downUrl = MobileApi.DOWNLOAD_BASE_URL + updateInfo.file;
		Intent inDownload = new Intent(Intent.ACTION_VIEW);
		inDownload.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
		inDownload.setData(Uri.parse(downUrl));
		context.startActivity(inDownload);
	}
}
