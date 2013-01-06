package com.rarnu.tools.root.service;

import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.util.Log;

import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.base.BaseService;
import com.rarnu.tools.root.common.Actions;
import com.rarnu.tools.root.utils.ApkUtils;

public class HtcRomService extends BaseService {
	
	private Intent inCleanHtc = new Intent(Actions.ACTION_CLEANING_HTC);

	private void deleteApplication(String namesapce) {
		try {
			ApplicationInfo info = GlobalInstance.pm.getApplicationInfo(
					namesapce, 0);
			String path = info.sourceDir;
			if (info.sourceDir.contains("/system/app/")) {
				ApkUtils.deleteSystemApp(path);
				ApkUtils.deleteSystemAppData(info.dataDir);
			} else {
				ApkUtils.uninstallApk(namesapce);
			}
		} catch (NameNotFoundException e) {
			Log.e("PackageNotFound", e.getMessage());
		}
	}

	@Override
	public void initIntent() {
		inCleanHtc.putExtra("operating", true);
		
	}

	@Override
	public void fiIntent() {
		inCleanHtc.removeExtra("operating");
		inCleanHtc.putExtra("operating", false);
		
	}

	@Override
	public Intent getSendIntent() {
		return inCleanHtc;
	}

	@Override
	public void doOperation(String command) {
		if (command.charAt(0) == '1') {
			deleteApplication("com.htc.AutoMotive");
			deleteApplication("com.htc.AutoMotive.Traffic");
			deleteApplication("com.htc.InternetRadio");
			deleteApplication("com.htc.autobot.cargps.provider");
		}

		if (command.charAt(1) == '1') {
			deleteApplication("com.htc.socialnetwork.facebook");
			deleteApplication("com.htc.engine.facebook");
			deleteApplication("com.facebook.katana");
		}

		if (command.charAt(2) == '1') {
			deleteApplication("com.htc.htctwitter");
			deleteApplication("com.htc.engine.twitter");
			deleteApplication("com.htc.Twitter3DWidget");
			deleteApplication("com.htc.Trends3DWidget");
			deleteApplication("com.twitter.android");
		}
		if (command.charAt(3) == '1') {
			deleteApplication("com.htc.dropbox.glrplugin");
			deleteApplication("com.htc.cloudstorage.dropbox");
			deleteApplication("com.dropbox.android");
		}
		if (command.charAt(4) == '1') {
			deleteApplication("com.htc.skydrive.glrplugin");
			deleteApplication("com.htc.cloudstorage.skydrive");
		}
		if (command.charAt(5) == '1') {
			deleteApplication("com.htc.laputa");
			deleteApplication("com.htc.laputa.HtcLaputaInstaller");
			deleteApplication("com.htc.laputa.widget3d.locations");
			deleteApplication("com.htc.laputa.widget3d.navigate");
			deleteApplication("com.htc.laputa.trip.TripWidget");
		}
		if (command.charAt(6) == '1') {
			deleteApplication("com.htc.socialnetwork.flickr");
			deleteApplication("com.htc.engine.flickr");
		}
		if (command.charAt(7) == '1') {
			deleteApplication("com.htc.friendstream");
			deleteApplication("com.htc.FriendStream3DWidget");
			deleteApplication("com.htc.idlescreen.socialnetwork");
		}
		if (command.charAt(8) == '1') {
			deleteApplication("com.google.android.apps.plus");
			deleteApplication("com.google.android.youtube");
			deleteApplication("com.htc.picasa");
			deleteApplication("com.google.android.gm");
			deleteApplication("com.google.android.voicesearch");
			deleteApplication("com.google.android.apps.genie.geniewidget");
		}
		if (command.charAt(9) == '1') {
			deleteApplication("com.adobe.flashplayer");
			deleteApplication("com.adobe.reader");
			deleteApplication("com.htc.pdfviewer");
			deleteApplication("com.infraware.docmaster");
			deleteApplication("com.htc.android.teeter");
		}
		
	}

	@Override
	public boolean getCommandCondition(String command) {
		return command != null && command.length() == 10;
	}

}
