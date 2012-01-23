package com.snda.root.bcm.utils;

import java.util.HashMap;
import java.util.Map;

import com.snda.root.bcm.R;

import android.content.Intent;

public class ReceiverMessageMap {

	public static Map<String, Integer> messageMap = new HashMap<String, Integer>();
	
	static {
		// a
		messageMap.put(Intent.ACTION_AIRPLANE_MODE_CHANGED, R.string.msg_intent_a1);
		messageMap.put(Intent.ACTION_ALARM_CHANGED, R.string.msg_intent_a2);
		messageMap.put(Intent.ACTION_ALL_APPS, R.string.msg_intent_a3);
		messageMap.put(Intent.ACTION_ANSWER, R.string.msg_intent_a4);
		messageMap.put(Intent.ACTION_APP_ERROR, R.string.msg_intent_a5);
		messageMap.put(Intent.ACTION_ATTACH_DATA, R.string.msg_intent_a6);
		// b
		messageMap.put(Intent.ACTION_BATTERY_CHANGED, R.string.msg_intent_b1);
		messageMap.put(Intent.ACTION_BATTERY_LOW, R.string.msg_intent_b2);
		messageMap.put(Intent.ACTION_BATTERY_OKAY, R.string.msg_intent_b3);
		messageMap.put(Intent.ACTION_BOOT_COMPLETED, R.string.msg_intent_b4);
		messageMap.put(Intent.ACTION_BUG_REPORT, R.string.msg_intent_b5);
		// c
		messageMap.put(Intent.ACTION_CALL, R.string.msg_intent_c1);
		messageMap.put(Intent.ACTION_CALL_BUTTON, R.string.msg_intent_c2);
		messageMap.put(Intent.ACTION_CALL_EMERGENCY, R.string.msg_intent_c3);
		messageMap.put(Intent.ACTION_CALL_PRIVILEGED, R.string.msg_intent_c4);
		messageMap.put(Intent.ACTION_CAMERA_BUTTON, R.string.msg_intent_c5);
		messageMap.put(Intent.ACTION_CHOOSER, R.string.msg_intent_c6);
		messageMap.put(Intent.ACTION_CLOSE_SYSTEM_DIALOGS, R.string.msg_intent_c7);
		messageMap.put(Intent.ACTION_CONFIGURATION_CHANGED, R.string.msg_intent_c8);
		messageMap.put(Intent.ACTION_CREATE_SHORTCUT, R.string.msg_intent_c9);
		// d
		messageMap.put(Intent.ACTION_DATE_CHANGED, R.string.msg_intent_d1);
		messageMap.put(Intent.ACTION_DEFAULT, R.string.msg_intent_d2);
		messageMap.put(Intent.ACTION_DELETE, R.string.msg_intent_d3);
		messageMap.put(Intent.ACTION_DEVICE_STORAGE_LOW, R.string.msg_intent_d4);
		messageMap.put(Intent.ACTION_DEVICE_STORAGE_OK, R.string.msg_intent_d5);
		messageMap.put(Intent.ACTION_DIAL, R.string.msg_intent_d6);
		messageMap.put(Intent.ACTION_DOCK_EVENT, R.string.msg_intent_d7);
		// e
		messageMap.put(Intent.ACTION_EDIT, R.string.msg_intent_e1);
		messageMap.put(Intent.ACTION_EXTERNAL_APPLICATIONS_AVAILABLE, R.string.msg_intent_e2);
		messageMap.put(Intent.ACTION_EXTERNAL_APPLICATIONS_UNAVAILABLE, R.string.msg_intent_e3);
		// f
		messageMap.put(Intent.ACTION_FACTORY_TEST, R.string.msg_intent_f1);
		// g
		messageMap.put(Intent.ACTION_GET_CONTENT, R.string.msg_intent_g1);
		messageMap.put(Intent.ACTION_GTALK_SERVICE_CONNECTED, R.string.msg_intent_g2);
		messageMap.put(Intent.ACTION_GTALK_SERVICE_DISCONNECTED, R.string.msg_intent_g3);
		// h
		messageMap.put(Intent.ACTION_HEADSET_PLUG, R.string.msg_intent_h1);
		// i
		messageMap.put(Intent.ACTION_INPUT_METHOD_CHANGED, R.string.msg_intent_i1);
		messageMap.put(Intent.ACTION_INSERT, R.string.msg_intent_i2);
		messageMap.put(Intent.ACTION_INSERT_OR_EDIT, R.string.msg_intent_i3);
		// l
		messageMap.put(Intent.ACTION_LOCALE_CHANGED, R.string.msg_intent_l1);
		// m
		messageMap.put(Intent.ACTION_MAIN, R.string.msg_intent_m1);
		messageMap.put(Intent.ACTION_MANAGE_PACKAGE_STORAGE, R.string.msg_intent_m2);
		messageMap.put(Intent.ACTION_MEDIA_BAD_REMOVAL, R.string.msg_intent_m3);
		messageMap.put(Intent.ACTION_MEDIA_BUTTON, R.string.msg_intent_m4);
		messageMap.put(Intent.ACTION_MEDIA_CHECKING, R.string.msg_intent_m5);
		messageMap.put(Intent.ACTION_MEDIA_EJECT, R.string.msg_intent_m6);
		messageMap.put(Intent.ACTION_MEDIA_MOUNTED, R.string.msg_intent_m7);
		messageMap.put(Intent.ACTION_MEDIA_NOFS, R.string.msg_intent_m8);
		messageMap.put(Intent.ACTION_MEDIA_REMOVED, R.string.msg_intent_m9);
		messageMap.put(Intent.ACTION_MEDIA_SCANNER_FINISHED, R.string.msg_intent_m10);
		messageMap.put(Intent.ACTION_MEDIA_SCANNER_SCAN_FILE, R.string.msg_intent_m11);
		messageMap.put(Intent.ACTION_MEDIA_SCANNER_STARTED, R.string.msg_intent_m12);
		messageMap.put(Intent.ACTION_MEDIA_SHARED, R.string.msg_intent_m13);
		messageMap.put(Intent.ACTION_MEDIA_UNMOUNTABLE, R.string.msg_intent_m14);
		messageMap.put(Intent.ACTION_MEDIA_UNMOUNTED, R.string.msg_intent_m15);
		messageMap.put(Intent.ACTION_MEDIA_UNSHARED, R.string.msg_intent_m16);
		// n
		messageMap.put(Intent.ACTION_NEW_OUTGOING_CALL, R.string.msg_intent_o1);
		// p
		messageMap.put(Intent.ACTION_PACKAGE_ADDED, R.string.msg_intent_p1);
		messageMap.put(Intent.ACTION_PACKAGE_CHANGED, R.string.msg_intent_p2);
		messageMap.put(Intent.ACTION_PACKAGE_DATA_CLEARED, R.string.msg_intent_p3);
		messageMap.put(Intent.ACTION_PACKAGE_INSTALL, R.string.msg_intent_p4);
		messageMap.put(Intent.ACTION_PACKAGE_REMOVED, R.string.msg_intent_p5);
		messageMap.put(Intent.ACTION_PACKAGE_REPLACED, R.string.msg_intent_p6);
		messageMap.put(Intent.ACTION_PACKAGE_RESTARTED, R.string.msg_intent_p7);
		messageMap.put(Intent.ACTION_PICK, R.string.msg_intent_p8);
		messageMap.put(Intent.ACTION_PICK_ACTIVITY, R.string.msg_intent_p9);
		messageMap.put(Intent.ACTION_POWER_CONNECTED, R.string.msg_intent_p10);
		messageMap.put(Intent.ACTION_POWER_DISCONNECTED, R.string.msg_intent_p11);
		messageMap.put(Intent.ACTION_POWER_USAGE_SUMMARY, R.string.msg_intent_p12);
		messageMap.put(Intent.ACTION_PRE_BOOT_COMPLETED, R.string.msg_intent_p13);
		messageMap.put(Intent.ACTION_PROVIDER_CHANGED, R.string.msg_intent_p14);
		// q
		messageMap.put(Intent.ACTION_QUERY_PACKAGE_RESTART, R.string.msg_intent_q1);
		// r
		messageMap.put(Intent.ACTION_REBOOT, R.string.msg_intent_r1);
		messageMap.put(Intent.ACTION_REMOTE_INTENT, R.string.msg_intent_r2);
		messageMap.put(Intent.ACTION_REQUEST_SHUTDOWN, R.string.msg_intent_r3);
		messageMap.put(Intent.ACTION_RUN, R.string.msg_intent_r4);
		// s
		messageMap.put(Intent.ACTION_SCREEN_OFF, R.string.msg_intent_s1);
		messageMap.put(Intent.ACTION_SCREEN_ON, R.string.msg_intent_s2);
		messageMap.put(Intent.ACTION_SEARCH, R.string.msg_intent_s3);
		messageMap.put(Intent.ACTION_SEARCH_LONG_PRESS, R.string.msg_intent_s4);
		messageMap.put(Intent.ACTION_SEND, R.string.msg_intent_s5);
		messageMap.put(Intent.ACTION_SEND_MULTIPLE, R.string.msg_intent_s6);
		messageMap.put(Intent.ACTION_SENDTO, R.string.msg_intent_s7);
		messageMap.put(Intent.ACTION_SET_WALLPAPER, R.string.msg_intent_s8);
		messageMap.put(Intent.ACTION_SHUTDOWN, R.string.msg_intent_s9);
		messageMap.put(Intent.ACTION_SYNC, R.string.msg_intent_s10);
		messageMap.put(Intent.ACTION_SYNC_STATE_CHANGED, R.string.msg_intent_s11);
		messageMap.put(Intent.ACTION_SYSTEM_TUTORIAL, R.string.msg_intent_s12);
		// t
		messageMap.put(Intent.ACTION_TIME_CHANGED, R.string.msg_intent_t1);
		messageMap.put(Intent.ACTION_TIME_TICK, R.string.msg_intent_t2);
		messageMap.put(Intent.ACTION_TIMEZONE_CHANGED, R.string.msg_intent_t3);
		// u
		messageMap.put(Intent.ACTION_UID_REMOVED, R.string.msg_intent_u1);
		messageMap.put(Intent.ACTION_UMS_CONNECTED, R.string.msg_intent_u2);
		messageMap.put(Intent.ACTION_UMS_DISCONNECTED, R.string.msg_intent_u3);
		messageMap.put(Intent.ACTION_UPGRADE_SETUP, R.string.msg_intent_u4);
		messageMap.put(Intent.ACTION_USER_PRESENT, R.string.msg_intent_u5);
		// v
		messageMap.put(Intent.ACTION_VIEW, R.string.msg_intent_v1);
		messageMap.put(Intent.ACTION_VOICE_COMMAND, R.string.msg_intent_v2);
		// w
		messageMap.put(Intent.ACTION_WALLPAPER_CHANGED, R.string.msg_intent_w1);
		messageMap.put(Intent.ACTION_WEB_SEARCH, R.string.msg_intent_w2);
		
		
		
	}
}
