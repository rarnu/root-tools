package com.rarnu.tools.root.api;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;

import org.apache.http.message.BasicNameValuePair;
import org.apache.http.protocol.HTTP;

import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.utils.HttpRequest;

public class LogApi {

	// [region] const define
	private static final String LOG_BASE_URL = "http://rarnu.7thgen.info/anjuke/root_tools/log.php";
	private static final String LOG_CRACH_URL = "http://rarnu.7thgen.info/anjuke/root_tools/crash.php";

	private static final String LOG_BASE_PARAM = "deviceId=%s&module=%s&os_version=%s&mail=%s&build_desc=%s&";
	private static final String LOG_ACTION = "action=%s";
	// [/region]

	// [region] varibale define
	private static String LOG_STATIC_PARAM = "";

	static {
		buildStaticParam();
	}

	// [/region]

	// [region] business logic
	public static void logAppFirstStart() {
		sendLog("onFirst");
	}

	public static void logAppStart() {
		sendLog("onStart");
	}

	public static void logAppStop() {
		sendLog("onStop");
	}

	public static void logEnterSysapp() {
		sendLog("onSysapp");
	}

	public static void logDeleteSystemApp(String namespace) {

		try {
			namespace = URLEncoder.encode(namespace, HTTP.UTF_8);
		} catch (UnsupportedEncodingException e) {

		}

		sendLog("onDeleteSystemApp." + namespace);
	}

	public static void logInstallSystemApp(String namespace) {
		
		try {
			namespace = URLEncoder.encode(namespace, HTTP.UTF_8);
		} catch (UnsupportedEncodingException e) {

		}
		sendLog("onInstallSystemApp." + namespace);
	}

	public static void logEnterData() {
		sendLog("onData");
	}

	public static void logBackupData() {
		sendLog("onBackupData");
	}

	public static void logRestoreData() {
		sendLog("onRestoreData");
	}

	public static void logDeleteData() {
		sendLog("onDeleteData");
	}

	public static void logDeleteAllData() {
		sendLog("onDeleteAllData");
	}

	public static void logEnterComponent() {
		sendLog("onComponent");
	}

	public static void logDisableComponent(String name) {
		try {
			name = URLEncoder.encode(name, HTTP.UTF_8);
		} catch (UnsupportedEncodingException e) {

		}

		sendLog("onDisableComponent." + name);
	}

	public static void logEnableComponent(String name) {
		
		try {
			name = URLEncoder.encode(name, HTTP.UTF_8);
		} catch (UnsupportedEncodingException e) {

		}
		sendLog("onEnableComponent." + name);
	}

	public static void logEnterProcess() {
		sendLog("onEnterProcess");
	}

	public static void logKillProcess(String namespace) {
		try {
			namespace = URLEncoder.encode(namespace, HTTP.UTF_8);
		} catch (UnsupportedEncodingException e) {

		}

		sendLog("onKillProcess." + namespace);
	}

	public static void logCleanMemory() {
		sendLog("onCleanMemory");
	}

	public static void logEnterDeleteIgnore() {
		sendLog("onEnterDeleteIgnore");
	}

	public static void logIgnoreProcess(String namespace) {
		
		try {
			namespace = URLEncoder.encode(namespace, HTTP.UTF_8);
		} catch (UnsupportedEncodingException e) {

		}
		sendLog("onIgnoreProcess." + namespace);
	}

	public static void logUnignoreProcess(String namespace) {
		
		try {
			namespace = URLEncoder.encode(namespace, HTTP.UTF_8);
		} catch (UnsupportedEncodingException e) {

		}
		sendLog("onUnignoreProcess." + namespace);
	}

	public static void logIgnoreList() {
		sendLog("onIgnoreList");
	}

	public static void logDeleteIgnore() {
		sendLog("onDeleteIgnore");
	}

	public static void logEnterHosts() {
		sendLog("onHosts");
	}

	public static void logAddHosts() {
		sendLog("onAddHosts");
	}

	public static void logSearchHosts(String url) {
		try {
			url = URLEncoder.encode(url, HTTP.UTF_8);
		} catch (UnsupportedEncodingException e) {

		}
		sendLog("onSearchHosts." + url);
	}

	public static void logDeleteHosts() {
		sendLog("onDeleteHosts");
	}

	public static void logEnterManualEditHosts() {
		sendLog("onEnterManualEditHosts");
	}

	public static void logManualEditHosts() {
		sendLog("onManualEditHosts");
	}

	public static void logEnterDeprecatedHosts() {
		sendLog("onEnterDeprecatedHosts");
	}

	public static void logCleanDeprecatedHosts() {
		sendLog("onCleanDeprecatedHosts");
	}

	public static void logScanMedia() {
		sendLog("onScanMedia");
	}

	public static void logEnterRootBusybox() {
		sendLog("onRootBusybox");
	}

	public static void logReinstallBusybox() {
		sendLog("onReinstallBusybox");
	}

	public static void logEnterSystemSettings() {
		sendLog("onSystemSettings");
	}

	public static void logCheckUpdate(int version) {
		sendLog("onCheckUpdate." + String.valueOf(version));
	}

	public static void logUserFeedback() {
		sendLog("onUserFeedback");
	}

	public static void logEnterAppRecommand() {
		sendLog("onAppRecommand");
	}

	public static void logEnterAbout() {
		sendLog("onAbout");
	}

	public static void logEnterHelp() {
		sendLog("onHelp");
	}

	public static void logEnterCache() {
		sendLog("onCache");
	}

	public static void logCleanCache() {
		sendLog("onCleanCache");
	}

	public static void logEnterMore() {
		sendLog("onMore");
	}

	public static void logCrash(String message) {
		try {
			message = URLEncoder.encode(message, HTTP.UTF_8);
			postCrashLog(message);
		} catch (Exception e) {
		}
	}

	// [/region]

	// [region] common
	private static void postCrashLog(final String crash) {
		new Thread(new Runnable() {

			@Override
			public void run() {
				List<BasicNameValuePair> params = new ArrayList<BasicNameValuePair>();
				params.add(new BasicNameValuePair("deviceId",
						GlobalInstance.deviceId));
				params.add(new BasicNameValuePair("module",
						GlobalInstance.module));
				params.add(new BasicNameValuePair("os_version",
						GlobalInstance.osVersion));
				params.add(new BasicNameValuePair("mail", GlobalInstance.mail));
				params.add(new BasicNameValuePair("build_desc",
						GlobalInstance.buildDescription));
				params.add(new BasicNameValuePair("crash", crash));
				HttpRequest.post(LOG_CRACH_URL, params, HTTP.UTF_8);
			}
		}).start();
	}

	private static void sendLog(final String params) {
		new Thread(new Runnable() {
			@Override
			public void run() {
				HttpRequest.get(LOG_BASE_URL,
						LOG_STATIC_PARAM + String.format(LOG_ACTION, params),
						HTTP.UTF_8);
			}
		}).start();
	}

	private static void buildStaticParam() {
		if (LOG_STATIC_PARAM.equals("")) {
			LOG_STATIC_PARAM = String.format(LOG_BASE_PARAM,
					GlobalInstance.deviceId, GlobalInstance.module,
					GlobalInstance.osVersion, GlobalInstance.mail,
					GlobalInstance.buildDescription);
		}
	}

	// [/region]
}
