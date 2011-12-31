package com.snda.root.bcm;

import android.app.Activity;
import android.content.pm.PackageParser;
import android.os.Bundle;
import android.widget.ImageView;
import android.widget.TextView;

public class PackageInfoActivity extends Activity {

	ImageView ivAppIcon;
	TextView tvAppName, tvAppPackage;

	TextView tvTestReceiver;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		setContentView(R.layout.packageinfo);

		ivAppIcon = (ImageView) findViewById(R.id.ivAppIcon);
		tvAppName = (TextView) findViewById(R.id.tvAppName);
		tvAppPackage = (TextView) findViewById(R.id.tvAppPackage);

		tvTestReceiver = (TextView) findViewById(R.id.tvTestReceiver);

		ivAppIcon.setBackgroundDrawable(GlobalInstance.currentPackageInfo.icon);
		tvAppName.setText(GlobalInstance.currentPackageInfo.label);
		tvAppPackage
				.setText(GlobalInstance.currentPackageInfo.pack.packageName);

		String ret = "";
		int i = 0;
		if (GlobalInstance.currentPackageInfo.receiverCount > 0) {
			for (PackageParser.Activity a : GlobalInstance.currentPackageInfo.pack.receivers) {
				ret += "\n\n" + a.info.name + "\n";
				if (a.intents != null) {
					if (a.intents.size() > 0) {
						for (PackageParser.ActivityIntentInfo aii : a.intents) {
							if (aii.countActions() > 0) {
								for (i = 0; i < aii.countActions(); i++) {
									ret += "    " + aii.getAction(i) + "\n";
								}
							}
						}
					}
				}
			}
		}
		tvTestReceiver.setText(ret);
	}
}
