package com.rarnu.tools.root.utils;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;

import com.rarnu.tools.root.utils.root.CommandResult;
import com.rarnu.tools.root.utils.root.RootUtils;

import android.content.Context;

public class BusyboxUtils {

	public static boolean isSuBusyboxReady() {
		boolean a = RootUtils.hasSu();
		boolean b = RootUtils.hasSuperuser();
		boolean c = RootUtils.hasBusybox();
		return (a && b && c);
	}

	public static boolean installBusybox(Context context) {

		CommandResult result = null;
		boolean ret = copyBusybox(context);
		if (!ret) {
			return false;
		}
		result = RootUtils.runCommand(String.format("cat %sbusybox > /system/xbin/busybox", DirHelper.BUSYBOX_DIR), true);
		result = RootUtils.runCommand("chmod 777 /system/xbin/busybox", true);
		result = RootUtils.runCommand("/system/xbin/busybox --install -s /system/xbin", true);
		if (!result.error.equals("")) {
			return false;
		}
		return true;

	}

	public static boolean installSuperuser(Context context, boolean isICS) {

		CommandResult result = null;
		boolean ret = copySuperuser(context, (isICS ? "ics" : "old"));
		if (!ret) {
			return false;
		}
		result = RootUtils.runCommand(String.format("cat %sSuperuser_%s.apk > /system/app/Superuser.apk",
				DirHelper.BUSYBOX_DIR, (isICS ? "ics" : "old")), true);
		if (!result.error.equals("")) {
			return false;
		}
		return true;

	}

	public static boolean removeBusybox() {

		CommandResult result = null;
		result = RootUtils.runCommand("busybox find /system/xbin -type l | busybox xargs rm -r", true);
		result = RootUtils.runCommand("rm /system/xbin/busybox", true);
		if (!result.error.equals("")) {
			return false;
		}

		return true;

	}

	public static boolean removeSuperuser() {

		RootUtils.runCommand("rm /system/app/Superuser.*", true);
		RootUtils.runCommand("rm /system/app/SuperSU.*", true);
		RootUtils.runCommand("rm /system/app/SuperSU_Pro.*", true);
		return true;
	}

	private static boolean copyBusybox(Context context) {
		File fBusybox = new File(DirHelper.BUSYBOX_DIR);
		if (!fBusybox.exists()) {
			fBusybox.mkdirs();
		}
		try {
			byte[] buffer = new byte[8192];

			File dest = new File(DirHelper.BUSYBOX_DIR + "busybox");

			if (dest.exists()) {
				dest.delete();
			}

			InputStream is = context.getAssets().open("busybox");
			OutputStream fos = new BufferedOutputStream(new FileOutputStream(dest));
			int n;
			while ((n = is.read(buffer, 0, buffer.length)) != -1)
				fos.write(buffer, 0, n);

			is.close();
			fos.close();
			return true;
		} catch (Exception ex) {
			return false;
		}
	}

	private static boolean copySuperuser(Context context, String surfix) {
		File fBusybox = new File(DirHelper.BUSYBOX_DIR);
		if (!fBusybox.exists()) {
			fBusybox.mkdirs();
		}
		try {
			byte[] buffer = new byte[8192];

			File dest = new File(DirHelper.BUSYBOX_DIR + "Superuser_" + surfix + ".apk");

			if (dest.exists()) {
				dest.delete();
			}

			InputStream is = context.getAssets().open("Superuser_" + surfix + ".apk");
			OutputStream fos = new BufferedOutputStream(new FileOutputStream(dest));
			int n;
			while ((n = is.read(buffer, 0, buffer.length)) != -1)
				fos.write(buffer, 0, n);

			is.close();
			fos.close();
			return true;
		} catch (Exception ex) {
			return false;
		}
	}

}
