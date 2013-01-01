package com.rarnu.tools.root.utils;

import android.content.Context;
import android.content.Intent;
import android.net.NetworkInfo;
import android.net.Uri;
import android.os.Environment;
import android.os.Handler;
import android.os.Message;
import android.preference.Preference;
import android.util.Log;
import android.view.View;
import android.widget.Toast;

import com.rarnu.root.pp4.R;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.comp.AlertDialogEx;

public class MiscUtils {

	public static void doScanMedia(Context context) {
		LogApi.logScanMedia();
		context.sendBroadcast(new Intent(Intent.ACTION_MEDIA_SCANNER_SCAN_FILE,
				Uri.parse("file://"
						+ Environment.getExternalStorageDirectory()
								.getAbsolutePath())));
		Log.e("Media Path", Environment.getExternalStorageDirectory()
				.getAbsolutePath());
		Toast.makeText(context, R.string.scan, Toast.LENGTH_LONG).show();
	}

	public static void doReboot(Context context) {
		AlertDialogEx.showAlertDialogEx(context,
				context.getString(R.string.reboot_device),
				context.getString(R.string.confirm_reboot),
				context.getString(R.string.ok),
				new AlertDialogEx.DialogButtonClickListener() {

					@Override
					public void onClick(View v) {
						DeviceUtils.reboot();

					}
				}, context.getString(R.string.cancel), null);
	}

	public static void doCleanDalvik(final Context context, final View view, final Preference pref) {

		pref.setTitle(R.string.cleaning_dalvik);
		view.setEnabled(false);
		
		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {

					pref.setTitle(R.string.clean_dalvik);
					view.setEnabled(true);
					
					if (msg.arg1 == -1) {
						Toast.makeText(context, R.string.clean_dalvik_fail,
								Toast.LENGTH_LONG).show();
					} else if (msg.arg1 == 0) {
						Toast.makeText(context, R.string.clean_dalvik_0,
								Toast.LENGTH_LONG).show();
					} else {
						Toast.makeText(
								context,
								String.format(context
										.getString(R.string.clean_dalvik_succ),
										msg.arg1), Toast.LENGTH_LONG).show();
					}
				}
				super.handleMessage(msg);
			}
		};
		new Thread(new Runnable() {

			@Override
			public void run() {
				Message msg = new Message();
				msg.what = 1;
				msg.arg1 = DalvikUtils.cleanDalvik();
				h.sendMessage(msg);

			}
		}).start();
	}

	public static void showNetworkStatus(final Context context) {

		if (GlobalInstance.loadingNetwork) {
			AlertDialogEx.showAlertDialogEx(context,
					context.getString(R.string.check_network_status),
					context.getString(R.string.loading_network_status),
					context.getString(R.string.ok), null, null, null);
			return;
		}

		String status = context.getString(R.string.no_connect_found);
		if (GlobalInstance.networkInfo != null) {

			status = String
					.format(context.getString(R.string.network_status_fmt),
							GlobalInstance.networkInfo.getTypeName(),
							GlobalInstance.networkInfo.getSubtypeName(),
							networkStatusToReadableString(context, GlobalInstance.networkInfo
									.getState()),

							(GlobalInstance.networkInfo.getExtraInfo() == null ? context
									.getString(R.string.not_contained)
									: GlobalInstance.networkInfo.getExtraInfo()),

							(GlobalInstance.networkInfo.isRoaming() ? context
									.getString(R.string.yes) : context
									.getString(R.string.no)),
							(GlobalInstance.networkInfo.isFailover() ? context
									.getString(R.string.supported) : context
									.getString(R.string.unsupported)),
							(GlobalInstance.networkInfo.isAvailable() ? context
									.getString(R.string.available) : context
									.getString(R.string.unavailable)),
							GlobalInstance.networkSpeed);
		}

		AlertDialogEx.showAlertDialogEx(context,
				context.getString(R.string.check_network_status), status,
				context.getString(R.string.ok), null, null, null);
	}

	private static String networkStatusToReadableString(Context context, NetworkInfo.State state) {
		switch (state) {
		case CONNECTED:
			return context.getString(R.string.network_connected);
		case CONNECTING:
			return context.getString(R.string.network_connecting);
		case DISCONNECTED:
			return context.getString(R.string.network_disconnected);
		case DISCONNECTING:
			return context.getString(R.string.network_disconnecting);
		case SUSPENDED:
			return context.getString(R.string.network_suspended);
		case UNKNOWN:
			return context.getString(R.string.network_unknown);

		}
		return context.getString(R.string.network_unknown);
	}
}
