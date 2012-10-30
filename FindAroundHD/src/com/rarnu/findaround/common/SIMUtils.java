package com.rarnu.findaround.common;

import android.app.Service;
import android.content.Context;
import android.telephony.TelephonyManager;

public class SIMUtils {

	public static boolean isSimCardReady(Context context) {
		TelephonyManager mTelephonyManager = (TelephonyManager) context
				.getSystemService(Service.TELEPHONY_SERVICE);
		return (mTelephonyManager.getSimState() == TelephonyManager.SIM_STATE_READY);

	}
}
