package com.rarnu.tools.root.receiver;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import com.rarnu.tools.root.utils.IptablesUtils;

public class PackageReceiver extends BroadcastReceiver {

	@Override
	public void onReceive(Context context, Intent intent) {
		if (Intent.ACTION_PACKAGE_REMOVED.equals(intent.getAction())) {
			boolean replacing = intent.getBooleanExtra(Intent.EXTRA_REPLACING, false);
			if (!replacing) {
				final int uid = intent.getIntExtra(Intent.EXTRA_UID, -123);
				IptablesUtils.applicationRemoved(context, uid);
				IptablesUtils.applications = null;
			}
		} else if (Intent.ACTION_PACKAGE_ADDED.equals(intent.getAction())) {
			IptablesUtils.applications = null;
		}
	}

}
